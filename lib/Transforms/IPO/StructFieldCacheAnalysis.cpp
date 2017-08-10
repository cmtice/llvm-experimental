// lib/Tranforms/IPO/StructFieldCacheAnalysis.cpp - Performs Cache-Aware
// Structure Analysis-*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===------------------------------------------------------------------------===//
//
// This file implements the main function of StructFieldCacheAnalysis pass. This
// pass will analyze struct field access patterns in a given program and prints
// recommendations on how to reorganize the structs to gain performance.
//
// The pass is implemented as following steps:
// 0. Prerequites: BlockFrequencyInfo and BranchProbabilityInfo
// 1. Perform IR analysis to obtain and record all memory accesses on which
//    field of all structs defined in the program. This step involves
//    interaction with class StructFieldAccessManager that calls functions in
//    class StructFieldAccessInfo to complete IR analysis.
// 2. Apply filters to structs and gather function arguments info. This step
//    will filter out some structs due to safety or performance concernts to
//    narrow down structs to be analyzed. It involves HotnessAnalyzer class.
//    This step also summarizes function calls that takes field address as
//    argument into function definitions to facilitate analysis.
// 3. Build close proximity graph for every pair of fields of each struct. It
//    needs to interact with StructFieldAccessManager to retrieve info for each
//    struct and create an object of CloseProximityBuilder to build CP
//    relations. The CloseProximityBuilder needs to build FieldReferenceGraph
//    object first to help build CP relations in steps.
// 4. Use the results of CP relations between fields and give suggestions to
//    either reorder struct fields or split structs by grouping fields into
//    smaller sub-structs. Depending on the flag specified by users, one or
//    both of FieldReorderTransformAnalyzer or StructSplitTransformAnalyzer
//    will be created to perform analysis and print suggestions.
//
// Meanwhile, this cpp file also implements the class of
// StructFieldAccessManager. It works like an organizer of all the informations
// used in the analysis. It organizes all struct access information as class of
// StructFieldAccessInfo per each struct with IR analysis and profiling info.
// With the struct access info, it then processes the info by creating a
// CloseProximityBuilder object for each struct and build close proximity for
// each pair of struct fields, which provides important information in making
// decision of struct reorganizations.
//
// There are other C++ files to complete this pass and they are organized as:
// Header file: lib/Tranforms/IPO/StructFieldCacheAnalysisImpl.h
//                    -- Includes definitions of all classes used in C++ files
// C++ files: lib/Tranforms/IPO/StructFieldCacheAnalysis.cpp
//            |   (Main file, defines main functions and
//            StructFieldAccessManager)
//            |
//            |-- lib/Transforms/IPO/StructFieldAccessInfo.cpp
//            |       (Implements StructFieldAccessInfo and HotnessAnalyzer)
//            |-- lib/Transforms/IPO/StructAnalysisCloseProximity.cpp
//            |       (Implements CloseProximityBuilder and FieldReferenceGraph)
//            |-- lib/Transforms/IPO/StructTransformAnalyzer.cpp
//                    (Implements FieldReorderAnalyzer and StructSplitAnalyzer)
//
// The tool is derived from the following paper:
//  M. Hagog, C. Tice “Cache Aware Data Layout Reorganization Optimization
//  in GCC”, Proceedings
//      of the GCC Developers’ Summit,  Ottawa, 2005.
//
//===------------------------------------------------------------------------===//

#include "StructFieldCacheAnalysisImpl.h"
#include "llvm/Support/FileSystem.h"

using namespace llvm;

#define DEBUG_TYPE "struct-analysis"
#define DEBUG_TYPE_IR "struct-analysis-IR"
#define DEBUG_TYPE_STATS "struct-analysis-detailed-stats"

static cl::opt<unsigned> MinimalAccessCountForAnalysis(
    "struct-analysis-minimal-count", cl::init(1), cl::Hidden,
    cl::desc("Minimal access count to make the struct eligible for analysis"));

static cl::opt<bool> PrintHistogramOnStructHotness(
    "struct-analysis-hotness-histogram", cl::init(false), cl::Hidden,
    cl::desc("If true, print a histogram on struct hotness"));

static cl::opt<unsigned> FilterOutSmallStructs(
    "struct-analysis-filter-small-structs", cl::init(1), cl::Hidden,
    cl::desc("If true, filter out structs with fewer number of fields than"
             "the specified number"));

static cl::opt<bool> PerformCodeAnalysisOnly(
    "struct-analysis-IR-only", cl::init(false), cl::Hidden,
    cl::desc("Stop the analysis after performing IR analysis"));

static cl::opt<bool> PerformCPGOnly(
    "struct-analysis-CPG-only", cl::init(false), cl::Hidden,
    cl::desc("Stop the analysis after performing CPG generation"));

static cl::opt<bool> EnableFieldReorderingSuggestions(
    "struct-analysis-suggest-reorder", cl::init(false), cl::Hidden,
    cl::desc("Give suggestions on reordering struct fields"));

static cl::opt<bool> EnableOldFieldReorderingSuggestions(
    "struct-analysis-suggest-reorder-old", cl::init(false), cl::Hidden,
    cl::desc("Give suggestions on reordering struct fields"));

static cl::opt<bool> EnableStructSplittingSuggestions(
    "struct-analysis-suggest-split", cl::init(false), cl::Hidden,
    cl::desc("Give suggestions on splitting structs into smaller ones"));

namespace llvm {
/// This class is inherited from AssemblyAnnotationWriter and used
/// to print annotated information on IR.
class StructFieldCacheAnalysisAnnotatedWriter
    : public AssemblyAnnotationWriter {
public:
  StructFieldCacheAnalysisAnnotatedWriter(
      const StructFieldAccessManager *S = nullptr)
      : StructManager(S) {}

  /// Override the base class function to print an annotate message after each
  /// basic block
  virtual void emitBasicBlockEndAnnot(const BasicBlock *BB,
                                      formatted_raw_ostream &OS) {
    OS.resetColor();
    auto count = StructManager->getExecutionCount(BB);
    if (count.hasValue()) {
      OS.changeColor(raw_ostream::YELLOW, false, false);
      OS << "; [prof count = " << count.getValue() << "]\n";
      OS.resetColor();
    } else {
      OS.changeColor(raw_ostream::YELLOW, false, false);
      OS << "; [prof count not found "
         << "]\n";
      OS.resetColor();
    }
  }

  /// Override the base class function to print an annotate message after each
  /// Instruction
  virtual void emitInstructionAnnot(const Instruction *I,
                                    formatted_raw_ostream &OS) {
    if (StructManager == nullptr)
      return;
    if (auto pair = StructManager->getFieldAccessOnInstruction(I)) {
      OS.changeColor(raw_ostream::GREEN, false, false);
      auto *type = pair.getValue().first;
      if (type->isLiteral())
        OS << "; [Field " << pair.getValue().second
           << " of a literal struct.] ";
      else
        OS << "; [Field " << pair.getValue().second << " of struct "
           << type->getStructName() << "] ";
    } else {
      OS.resetColor();
    }
  }

private:
  const StructFieldAccessManager *StructManager;
};
} // namespace llvm

// Functions for StructFieldAccessManager
StructFieldAccessManager::~StructFieldAccessManager() {
  delete HotnessAnalyzer;
  for (auto &it : StructFieldAccessInfoMap)
    delete it.second;
  for (auto &it : CloseProximityBuilderMap)
    delete it.second;
  StructFieldAccessInfoMap.clear();
  CloseProximityBuilderMap.clear();
  DEBUG(dbgs() << "Finish StructFieldAccessManager destructor\n");
}

// Create a StructFieldAccessInfo for the specified struct type T
// if never seen before. Otherwise return the previously created
// object
StructFieldAccessInfo *
StructFieldAccessManager::createOrGetStructFieldAccessInfo(
    const Type *T, const StructDefinitionType SType) {
  assert(isa<StructType>(T));
  auto *ST = cast<StructType>(T);
  if (auto *def = getStructFieldAccessInfo(ST))
    return def;
  else {
    // FIXME: retrieve debug info of the struct first: auto* debugInfo =
    // retrieveDebugInfoForStruct(T);
    def = StructFieldAccessInfoMap[ST] =
        new StructFieldAccessInfo(ST, SType, CurrentModule, this, nullptr);
    return def;
  }
}

// Get a previous created StructFieldAccessInfo object
StructFieldAccessInfo *
StructFieldAccessManager::getStructFieldAccessInfo(const Type *T) const {
  if (!isa<StructType>(T))
    return nullptr;
  auto *ST = cast<StructType>(T);
  auto ret = StructFieldAccessInfoMap.find(ST);
  if (ret != StructFieldAccessInfoMap.end())
    return ret->second;
  else
    return nullptr;
}

// Get a pair of the struct type and field number that the instruction
// is accessed, if any. Otherwise return None of the Optional type
Optional<StructInfoMapPairType>
StructFieldAccessManager::getFieldAccessOnInstruction(
    const Instruction *I) const {
  for (auto &it : StructFieldAccessInfoMap) {
    Optional<FieldNumType> FieldNum;
    Optional<ArgNumType> ArgNum;
    it.second->getAccessFieldNumOrArgNum(I, FieldNum, ArgNum);
    if (FieldNum) {
      return std::make_pair(it.first, FieldNum.getValue());
    }
  }
  return None;
}

// Function used to count how many struct types that only appears in the field
// of another struct type (so never created a StructFieldAccessInfo object for
// it). Used to count the opportunites of suggestions we missed
void StructFieldAccessManager::countIgnoredStructsInField() {
  std::unordered_set<StructType *> IgnoredStructTypes; // Make sure struct
  // type is not counted multiple times
  for (auto &it : StructFieldAccessInfoMap) {
    for (unsigned i = 0; i < it.first->getNumElements(); i++) {
      auto *Ty = it.first->getElementType(i);
      StructType *StructTypeToFind = nullptr;
      if (Ty->isStructTy()) {
        StructTypeToFind = cast<StructType>(Ty);
      } else if (Ty->isPointerTy() &&
                 Ty->getPointerElementType()->isStructTy()) {
        StructTypeToFind = cast<StructType>(Ty->getPointerElementType());
      } else if (Ty->isArrayTy() && Ty->getArrayElementType()->isStructTy()) {
        StructTypeToFind = cast<StructType>(Ty->getArrayElementType());
      } else if (Ty->isPointerTy() &&
                 Ty->getPointerElementType()->isPointerTy() &&
                 Ty->getPointerElementType()
                     ->getPointerElementType()
                     ->isStructTy()) {
        addStats(DebugStats::DS_StructPtrPtrAsField);
      }
      if (StructTypeToFind && IgnoredStructTypes.find(StructTypeToFind) ==
                                  IgnoredStructTypes.end()) {
        auto it = StructFieldAccessInfoMap.find(StructTypeToFind);
        if (it == StructFieldAccessInfoMap.end()) {
          addStats(DebugStats::DS_StructAsFieldIgnored);
          IgnoredStructTypes.insert(StructTypeToFind);
        }
      }
    }
  }
}

// Main function to call summarizeFunctionCalls for all StructFieldAccessInfo
void StructFieldAccessManager::summarizeFunctionCalls() {
  for (auto &it : StructFieldAccessInfoMap) {
    it.second->summarizeFunctionCalls();
  }
}

// Apply several filters to the structs to narrow down the number of structs
// to analyze
void StructFieldAccessManager::applyFiltersToStructs() {
  // TODO: This function needs more work to add more filters to reduce the
  // number of structs for analysis
  auto RemoveEntry =
      [&](std::unordered_map<const StructType *,
                             StructFieldAccessInfo *>::iterator &it) {
        delete it->second;
        auto ToRemove = it++;
        StructFieldAccessInfoMap.erase(ToRemove);
      };

  for (auto it = StructFieldAccessInfoMap.begin();
       it != StructFieldAccessInfoMap.end();) {
    if (!it->second->isEligible()) {
      RemoveEntry(it);
      addStats(DebugStats::DS_PassedIntoOutsideFunction);
    } else if (it->second->getTotalNumFieldAccess() <
               MinimalAccessCountForAnalysis) {
      RemoveEntry(it);
      addStats(DebugStats::DS_NoAccess);
    } else if (it->second->getNumElements() < FilterOutSmallStructs) {
      RemoveEntry(it);
      addStats(DebugStats::DS_StructTooSmall);
    } else {
      HotnessAnalyzer->addStruct(it->second);
      it++;
    }
  }

  if (PrintHistogramOnStructHotness)
    HotnessAnalyzer->generateHistogram();

  for (auto it = StructFieldAccessInfoMap.begin();
       it != StructFieldAccessInfoMap.end();) {
    if (!HotnessAnalyzer->isHot(it->second)) {
      RemoveEntry(it);
      addStats(DebugStats::DS_FilterColdStructs);
    } else {
      it++;
    }
  }
}

// Main function to build CloseProximityRelations by creating a
// CloseProximityBuilder for each struct type
void StructFieldAccessManager::buildCloseProximityRelations() {
  for (auto &it : StructFieldAccessInfoMap) {
    auto *CPB = new CloseProximityBuilder(CurrentModule, this, it.second);
    CPB->buildCloseProximityRelations();
    CloseProximityBuilderMap[it.first] = CPB;
  }
}

// Main function to suggest field reordering
void StructFieldAccessManager::suggestFieldReordering(bool UseOld) {
  outs() << "------------- Suggestions on Reordering ------------------\n";
  for (auto &it : CloseProximityBuilderMap) {
    auto *type = it.first;
    if (type->isLiteral())
      outs() << "Recommendation on an anonymous struct ";
    else
      outs() << "Recommendation on struct [" << type->getStructName() << "] ";
    auto Hotness = HotnessAnalyzer->getHotness(type);
    assert(Hotness);
    outs() << " (Hotness "
           << 100 * Hotness.getValue() / HotnessAnalyzer->getMaxHotness()
           << "% of hottest struct):\n";
    if (UseOld) {
      OldFieldReorderAnalyzer OFRA(CurrentModule, type, it.second, nullptr);
      OFRA.makeSuggestions();
    } else {
      NewFieldReorderAnalyzer NFRA(CurrentModule, type, it.second, nullptr);
      NFRA.makeSuggestions();
    }
  }
  outs() << "----------------------------------------------------------\n";
}

// Main function to suggest struct splitting
void StructFieldAccessManager::suggestStructSplitting() {
  outs() << "------------- Suggestions on Splitting ------------------\n";
  for (auto &it : CloseProximityBuilderMap) {
    auto *type = it.first;
    if (type->isLiteral()) {
      outs() << "Recommendation on an anonymous struct ";
    } else {
      outs() << "Recommendation on struct [" << type->getStructName() << "] ";
    }
    auto Hotness = HotnessAnalyzer->getHotness(type);
    assert(Hotness);
    outs() << " (Hotness "
           << 100 * Hotness.getValue() / HotnessAnalyzer->getMaxHotness()
           << "% of hottest struct):\n";
    StructSplitAnalyzer SSA(CurrentModule, type, it.second, nullptr);
    SSA.makeSuggestions();
  }
  outs() << "----------------------------------------------------------\n";
}

// (Debug only) Main function to print all the struct field accesses for all
// the struct types
void StructFieldAccessManager::debugPrintAllStructAccesses() {
  dbgs() << "------------ Printing all struct accesses: ---------------- \n";
  for (auto &it : StructFieldAccessInfoMap) {
    dbgs().changeColor(raw_ostream::YELLOW);
    auto *type = it.first;
    if (type->isLiteral()) {
      dbgs() << "A literal struct has accesses: \n";
    } else {
      dbgs() << "Struct [" << type->getStructName() << "] has accesses: \n";
    }
    dbgs().changeColor(raw_ostream::GREEN);
    it.second->debugPrintAllStructAccesses(dbgs());
    dbgs().resetColor();
  }
  dbgs() << "----------------------------------------------------------- \n";
}

// (Debug only) Main function to print all CPGs
void StructFieldAccessManager::debugPrintAllCPGs() const {
  dbgs() << "------------ Printing all CPGs: ------------------- \n";
  for (auto &it : CloseProximityBuilderMap) {
    dbgs().changeColor(raw_ostream::YELLOW);
    auto *type = it.first;
    assert(isa<StructType>(type));
    if (dyn_cast<StructType>(type)->isLiteral()) {
      dbgs() << "A literal struct has CPG: \n";
    } else {
      dbgs() << "Struct [" << type->getStructName() << "] has CPG: \n";
    }
    dbgs().changeColor(raw_ostream::GREEN);
    it.second->debugPrintCloseProximityGraph(dbgs());
    dbgs().resetColor();
  }
  dbgs() << "----------------------------------------------------------- \n";
}

// (Debug only) Print annotated IR to a file that has all the field accesses
// marked with the struct type and field number it accesses
void StructFieldAccessManager::debugPrintAnnotatedModule() {
  StructFieldCacheAnalysisAnnotatedWriter Writer(this);
  std::error_code EC;
  raw_fd_ostream FILE_OS("AnnotatedModule.IR.ll", EC, llvm::sys::fs::F_RW);
  FILE_OS.changeColor(raw_ostream::YELLOW);
  FILE_OS << "Annotated module print\n";
  FILE_OS.resetColor();
  CurrentModule.print(FILE_OS, &Writer);
  FILE_OS.resetColor();
}

// (Debug only) Print statistics
void StructFieldAccessManager::printStats() {
  std::error_code EC;
  outs() << "------------ Printing stats for struct accesses: "
            "---------------- \n";
  outs().changeColor(raw_ostream::YELLOW);
  outs() << "There are " << StructFieldAccessInfoMap.size()
         << " struct types are accessed in the program\n";
  for (auto &it : StructFieldAccessInfoMap) {
    auto *type = it.first;
    auto Result = it.second->getTotalNumFieldAccess();
    assert(Result);
    auto Hotness = it.second->calculateTotalHotness();
    if (type->isLiteral()) {
      outs() << "A literal struct defined as "
             << StructDefinitionTypeNames[it.second->getStructDefinition()]
             << " has " << Result << " accesses and " << Hotness
             << " execution count.\n";
    } else {
      outs() << "Struct [" << type->getStructName() << "] defined as "
             << StructDefinitionTypeNames[it.second->getStructDefinition()]
             << " has " << Result << " accesses and " << Hotness
             << " execution count.\n";
    }
  }
  outs().resetColor();
  outs().changeColor(raw_ostream::GREEN);
  outs() << "Stats:\n";
  for (auto &it : StructFieldAccessInfoMap) {
    for (auto i = 0; i < DebugStats::DS_MaxNumStats; i++) {
      StatCounts[i] += it.second->getStats(i);
      if (i == DebugStats::DS_GepUnknownUse) {
        it.second->printUnknownOpcodes(outs());
      }
    }
  }
  for (auto i = 0; i < DebugStats::DS_MaxNumStats; i++) {
    if (StatCounts[i]) {
      outs() << "Case " << StatNames[i] << " was found " << StatCounts[i]
             << " times\n";
    }
  };
  outs().resetColor();
  outs() << "----------------------------------------------------------------"
            "- \n";
}

// Helper function to decide which StructFieldAccessInfo to call based on the
// definition type of a struct (struct, struct* or struct [])
static void analyzeStructDefinitions(StructFieldAccessManager *StructManager,
                                     Value *Definition) {
  assert(isa<GlobalValue>(Definition) || isa<AllocaInst>(Definition));
  const StructFieldAccessManager::StructDefinitionType GlobalDefinitionTypes[] =
      {StructFieldAccessManager::StructDefinitionType::SDT_GlobalStruct,
       StructFieldAccessManager::StructDefinitionType::SDT_GlobalStructPtr,
       StructFieldAccessManager::StructDefinitionType::SDT_GlobalStructArray};
  const StructFieldAccessManager::StructDefinitionType LocalDefinitionTypes[] =
      {StructFieldAccessManager::StructDefinitionType::SDT_LocalStruct,
       StructFieldAccessManager::StructDefinitionType::SDT_LocalStructPtr,
       StructFieldAccessManager::StructDefinitionType::SDT_LocalStructArray};

  Type *DefType;
  const StructFieldAccessManager::StructDefinitionType *DefinitionTypes;
  StringRef TypeName;
  if (isa<GlobalValue>(Definition)) {
    DefType = cast<GlobalValue>(Definition)->getValueType();
    DefinitionTypes = GlobalDefinitionTypes;
    TypeName = "global";
  } else {
    DefType = cast<AllocaInst>(Definition)->getAllocatedType();
    DefinitionTypes = LocalDefinitionTypes;
    TypeName = "local";
  }
  if (DefType->isStructTy()) {
    DEBUG_WITH_TYPE(DEBUG_TYPE_IR, dbgs() << "Found a " << TypeName
                                          << " defined as struct: " << *DefType
                                          << "\n");
    auto *StructInfoPtr = StructManager->createOrGetStructFieldAccessInfo(
        DefType, DefinitionTypes[0]);
    StructInfoPtr->analyzeUsersOfStructValue(Definition);
  }
  // Case for struct*
  else if (DefType->isPointerTy() &&
           DefType->getPointerElementType()->isStructTy()) {
    DEBUG_WITH_TYPE(DEBUG_TYPE_IR, dbgs() << "Found a " << TypeName
                                          << " has struct* type: " << *DefType
                                          << "\n");
    auto *StructInfoPtr = StructManager->createOrGetStructFieldAccessInfo(
        DefType->getPointerElementType(), DefinitionTypes[1]);
    StructInfoPtr->analyzeUsersOfStructPointerValue(Definition);
  }
  // Case for struct []
  else if (DefType->isArrayTy() &&
           DefType->getArrayElementType()->isStructTy()) {
    DEBUG_WITH_TYPE(DEBUG_TYPE_IR, dbgs() << "Found a " << TypeName
                                          << " has struct[] type: " << *DefType
                                          << "\n");
    auto *StructInfoPtr = StructManager->createOrGetStructFieldAccessInfo(
        DefType->getArrayElementType(), DefinitionTypes[2]);
    StructInfoPtr->analyzeUsersOfStructArrayValue(Definition);
  }
  // Case for struct**
  else if (DefType->isPointerTy() &&
           DefType->getPointerElementType()->isPointerTy() &&
           DefType->getPointerElementType()
               ->getPointerElementType()
               ->isStructTy()) {
    DEBUG_WITH_TYPE(DEBUG_TYPE_IR, dbgs() << "Found a " << TypeName
                                          << " has struct** type: " << *DefType
                                          << " but we ignored this\n");
    StructManager->addStats(
        StructFieldAccessManager::DebugStats::DS_StructPtrPtr);
  }
}

// Main function to perform program (IR) analysis by finding struct/class
// variables defined as a global or local variable. Also track of uses of
// function arguments that are of struct types (step 1)
static void performIRAnalysis(Module &M,
                              StructFieldAccessManager *StructManager) {
  // Find all global structs
  for (auto &G : M.globals()) {
    // Only process globals defined in current module (the scope of whole
    // program)
    if (G.isDeclaration())
      continue;
    DEBUG_WITH_TYPE(DEBUG_TYPE_IR, dbgs().changeColor(raw_ostream::YELLOW));
    analyzeStructDefinitions(StructManager, &G);
    DEBUG_WITH_TYPE(DEBUG_TYPE_IR, dbgs().resetColor());
  }

  // Find all structs declared by allocas
  for (auto &F : M) {
    if (F.isDeclaration())
      continue;
    // Find all alloca of structs inside the function body
    for (auto &BB : F) {
      for (auto &I : BB) {
        if (isa<AllocaInst>(I)) {
          analyzeStructDefinitions(StructManager, &I);
        }
      }
    }
  }
  // Find uses of structs through function calls
  for (auto &F : M) {
    if (F.isDeclaration())
      continue;
    unsigned ArgNum = 0;
    for (auto &AG : F.args()) {
      if (AG.getType()->isStructTy()) {
        DEBUG_WITH_TYPE(DEBUG_TYPE_IR,
                        dbgs()
                            << "Found an argument of a struct pass by value: "
                            << AG << " and no support for this yet\n");
        StructManager->addStats(
            StructFieldAccessManager::DebugStats::DS_FuncArgValue);
      } else if (AG.getType()->isPointerTy()) {
        // AG is an address of a struct
        if (AG.getType()->getPointerElementType()->isStructTy()) {
          // Identified AG is an argument with a struct type
          auto *StructPtr = StructManager->getStructFieldAccessInfo(
              AG.getType()->getPointerElementType());
          if (StructPtr) {
            DEBUG_WITH_TYPE(
                DEBUG_TYPE_IR,
                dbgs()
                    << "Found an argument of a struct defined in the module: "
                    << AG << "\n");
            StructPtr->analyzeUsersOfStructValue(&AG);
          } else {
            DEBUG_WITH_TYPE(DEBUG_TYPE_IR,
                            dbgs() << "Found an argument of a struct "
                                      "not defined in the program: "
                                   << AG << "\n");
            StructManager->addStats(
                StructFieldAccessManager::DebugStats::DS_FuncArgNotDefined);
          }
        } else {
          // AG is a pointer, which could points to a field address when
          // the function is called
          for (auto *User : AG.users()) {
            if (isa<LoadInst>(User)) {
              StructManager->addLoadStoreArgAccess(cast<Instruction>(User),
                                                   ArgNum);
            } else if (isa<StoreInst>(User)) {
              if (cast<StoreInst>(User)->getPointerOperand() == &AG)
                StructManager->addLoadStoreArgAccess(cast<Instruction>(User),
                                                     ArgNum);
              else
                StructManager->addStats(StructFieldAccessManager::DebugStats::
                                            DS_FuncArgStoredInAnotherVariable);
            }
          }
        }
      }
      ArgNum++;
    }
  }
  // Add stats to count struct as a field of another struct
  StructManager->countIgnoredStructsInField();
  // Summarizes all uses of fields in function calls
  DEBUG_WITH_TYPE(DEBUG_TYPE_IR, StructManager->debugPrintAllStructAccesses());
  DEBUG_WITH_TYPE(DEBUG_TYPE_IR, StructManager->debugPrintAnnotatedModule());
}

// Wrapper function to apply filters (step 2)
static void applyFilters(StructFieldAccessManager *StructManager) {
  StructManager->summarizeFunctionCalls();
  StructManager->applyFiltersToStructs();
  StructManager->printStats();
}

// Wrapper function to build CP relations (step 3)
static void
buildCloseProximityRelations(StructFieldAccessManager *StructManager) {
  if (!PerformCodeAnalysisOnly) {
    StructManager->buildCloseProximityRelations();
    if (PerformCPGOnly)
      StructManager->debugPrintAllCPGs();
    else
      DEBUG(StructManager->debugPrintAllCPGs());
  }
}

// Wrapper function to make suggestions (step 4)
static void giveSuggestions(StructFieldAccessManager *StructManager) {
  if (EnableFieldReorderingSuggestions)
    StructManager->suggestFieldReordering(false);
  if (EnableOldFieldReorderingSuggestions)
    StructManager->suggestFieldReordering(true);
  if (EnableStructSplittingSuggestions)
    StructManager->suggestStructSplitting();
}

static bool performStructFieldCacheAnalysis(
    Module &M, function_ref<BlockFrequencyInfo *(Function &)> LookupBFI,
    function_ref<BranchProbabilityInfo *(Function &)> LookupBPI) {
  DEBUG(dbgs() << "Start of struct field cache analysis\n");
  StructFieldAccessManager StructManager(M, LookupBFI, LookupBPI);
  // Step 0 - retrieve debug info for all struct TODO: disable for now because
  // it's not supporting annonymous structs
  // Step 1 - perform IR analysis to collect info of all structs
  performIRAnalysis(M, &StructManager);
  // Step 2 - summarize function calls and apply filters
  applyFilters(&StructManager);
  // Step 3 - build and collapse Field Reference Graph and create Close
  // Proximity Graph
  buildCloseProximityRelations(&StructManager);
  // Step 4 - make suggestions
  giveSuggestions(&StructManager);

  DEBUG(dbgs() << "End of struct field cache analysis\n");
  return false;
}

namespace {
class StructFieldCacheAnalysisPass : public ModulePass {
public:
  static char ID;
  StructFieldCacheAnalysisPass() : ModulePass(ID) {
    initializeStructFieldCacheAnalysisPassPass(
        *PassRegistry::getPassRegistry());
  }

private:
  bool runOnModule(Module &M) override;
  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.addRequired<BlockFrequencyInfoWrapperPass>();
  }
};
} // namespace

char StructFieldCacheAnalysisPass::ID = 0;
INITIALIZE_PASS_BEGIN(StructFieldCacheAnalysisPass,
                      "struct-field-cache-analysis",
                      "Struct Field Cache Analysis", false, false)
INITIALIZE_PASS_DEPENDENCY(BlockFrequencyInfoWrapperPass)
INITIALIZE_PASS_DEPENDENCY(BranchProbabilityInfoWrapperPass)
INITIALIZE_PASS_END(StructFieldCacheAnalysisPass, "struct-field-cache-analysis",
                    "Struct Field Cache Analysis", false, false)
ModulePass *llvm::createStructFieldCacheAnalysisPass() {
  return new StructFieldCacheAnalysisPass;
}
StructFieldCacheAnalysis::StructFieldCacheAnalysis() {}

PreservedAnalyses StructFieldCacheAnalysis::run(Module &M,
                                                ModuleAnalysisManager &AM) {
  auto &FAM = AM.getResult<FunctionAnalysisManagerModuleProxy>(M).getManager();
  auto LookupBFI = [&FAM](Function &F) {
    return &FAM.getResult<BlockFrequencyAnalysis>(F);
  };
  auto LookupBPI = [&FAM](Function &F) {
    return &FAM.getResult<BranchProbabilityAnalysis>(F);
  };
  if (!performStructFieldCacheAnalysis(M, LookupBFI, LookupBPI))
    return PreservedAnalyses::all();
  return PreservedAnalyses::none();
}

bool StructFieldCacheAnalysisPass::runOnModule(Module &M) {
  auto LookupBFI = [this](Function &F) {
    return &this->getAnalysis<BlockFrequencyInfoWrapperPass>(F).getBFI();
  };
  auto LookupBPI = [this](Function &F) {
    return &this->getAnalysis<BranchProbabilityInfoWrapperPass>(F).getBPI();
  };
  return performStructFieldCacheAnalysis(M, LookupBFI, LookupBPI);
}
