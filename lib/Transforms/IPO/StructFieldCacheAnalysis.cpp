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
// 4. (Not implemented) Use the results of CP relations between fields and give
//    suggestions to either reorder struct fields or split structs by grouping
//    fields into smaller sub-structs. Depending on the flag specified by users,
//    one or both of FieldReorderTransformAnalyzer or
//    StructSplitTransformAnalyzer will be created to perform analysis and print
//    suggestions.
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
//                    (Implements CloseProximityBuilder and FieldReferenceGraph)
//
// The tool is derived from the following paper:
//  [1] M. Hagog, C. Tice “Cache Aware Data Layout Reorganization Optimization
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

static cl::opt<bool> PerformCodeAnalysisOnly(
    "struct-analysis-IR-only", cl::init(false), cl::Hidden,
    cl::desc("Stop the analysis after performing IR analysis"));

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

Optional<StructInfoMapPairType>
StructFieldAccessManager::getFieldAccessOnInstruction(
    const Instruction *I) const {
  Optional<StructInfoMapPairType> ret;
  for (auto &it : StructFieldAccessInfoMap) {
    if (auto FieldNum = it.second->getAccessFieldNum(I)) {
      return std::make_pair(it.first, FieldNum.getValue());
    }
  }
  return ret;
}

void StructFieldAccessManager::summarizeFunctionCalls() {
  for (auto &it : StructFieldAccessInfoMap) {
    it.second->summarizeFunctionCalls();
  }
}

void StructFieldAccessManager::applyFiltersToStructs() {
  // TODO: This function needs more work to add more filters to reduce the
  // number of structs for analysis
  DEBUG_WITH_TYPE(DEBUG_TYPE_IR, dbgs() << "To apply filters to structs\n");
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
    } else {
      HotnessAnalyzer->addStruct(it->second);
      it++;
    }
  }

  DEBUG_WITH_TYPE(DEBUG_TYPE_STATS, HotnessAnalyzer->generateHistogram());
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

void StructFieldAccessManager::buildCloseProximityRelations() {
  for (auto &it : StructFieldAccessInfoMap) {
    auto *CPB = new CloseProximityBuilder(CurrentModule, this, it.second);
    CPB->buildCloseProximityRelations();
    CloseProximityBuilderMap[it.first] = CPB;
  }
}

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

void StructFieldAccessManager::debugPrintAllCPGs() const {
  dbgs() << "------------ Printing all CPGs: ------------------- \n";
  for (auto &it : CloseProximityBuilderMap) {
    dbgs().changeColor(raw_ostream::YELLOW);
    auto *type = it.first;
    assert(isa<StructType>(type));
    if (dyn_cast<StructType>(type)->isLiteral()) {
      dbgs() << "A literal struct has CPG: \n";
    } else {
      dbgs() << "Struct [" << type->getStructName() << "] has FRG: \n";
    }
    dbgs().changeColor(raw_ostream::GREEN);
    it.second->debugPrintCloseProximityGraph(dbgs());
    dbgs().resetColor();
  }
  dbgs() << "----------------------------------------------------------- \n";
}

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

void StructFieldAccessManager::printStats() {
  std::error_code EC;
  raw_fd_ostream FILE_OS("/tmp/SFCA-" + CurrentModule.getName().str() + ".csv",
                         EC, llvm::sys::fs::F_RW);
  FILE_OS << "Name," << CurrentModule.getName() << "\n";
  outs()
      << "------------ Printing stats for struct accesses: ---------------- \n";
  outs().changeColor(raw_ostream::YELLOW);
  outs() << "There are " << StructFieldAccessInfoMap.size()
         << " struct types are accessed in the program\n";
  FILE_OS << "Total," << StructFieldAccessInfoMap.size() << "\n";
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
      FILE_OS << "Literal," << Result << "\n";
    } else {
      outs() << "Struct [" << type->getStructName() << "] defined as "
             << StructDefinitionTypeNames[it.second->getStructDefinition()]
             << " has " << Result << " accesses and " << Hotness
             << " execution count.\n";
      FILE_OS << type->getStructName() << "," << Result << "\n";
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
  outs().changeColor(raw_ostream::BLUE);
  outs() << "Stats are stored into "
         << "/tmp/SFCA-" + CurrentModule.getName().str() + ".csv"
         << "\n";
  outs().resetColor();
  outs()
      << "----------------------------------------------------------------- \n";
  FILE_OS.close();
}

static void performIRAnalysis(Module &M,
                              StructFieldAccessManager *StructManager) {
  // Find all global structs
  for (auto &G : M.globals()) {
    // Only process globals defined in current module (the scope of whole
    // program)
    if (G.isDeclaration())
      continue;
    DEBUG_WITH_TYPE(DEBUG_TYPE_IR, dbgs().changeColor(raw_ostream::YELLOW));
    // G is always a pointer
    if (G.getValueType()->isStructTy()) {
      DEBUG_WITH_TYPE(DEBUG_TYPE_IR,
                      dbgs()
                          << "Found a global defined as struct: " << G << "\n");
      auto *structInfoPtr = StructManager->createOrGetStructFieldAccessInfo(
          G.getValueType(),
          StructFieldAccessManager::StructDefinitionType::SDT_GlobalStruct);
      structInfoPtr->analyzeUsersOfStructValue(&G);
    }
    // Case for struct*
    else if (G.getValueType()->isPointerTy() &&
             G.getValueType()->getPointerElementType()->isStructTy()) {
      DEBUG_WITH_TYPE(DEBUG_TYPE_IR,
                      dbgs()
                          << "Found a global has struct* type: " << G << "\n");
      auto *structInfoPtr = StructManager->createOrGetStructFieldAccessInfo(
          G.getValueType()->getPointerElementType(),
          StructFieldAccessManager::StructDefinitionType::SDT_GlobalStructPtr);
      structInfoPtr->analyzeUsersOfStructPointerValue(&G);
    }
    // Case for struct**
    else if (G.getType()->isPointerTy() &&
             G.getType()->getPointerElementType()->isPointerTy() &&
             G.getType()
                 ->getPointerElementType()
                 ->getPointerElementType()
                 ->isStructTy()) {
      DEBUG_WITH_TYPE(DEBUG_TYPE_IR,
                      dbgs() << "Found a global has struct** type: " << G
                             << " but we ignored this\n");
      StructManager->addStats(
          StructFieldAccessManager::DebugStats::DS_StructPtrPtr);
    }
    DEBUG_WITH_TYPE(DEBUG_TYPE_IR, dbgs().resetColor());
  }

  // Find all structs declared by allocas
  for (auto &F : M) {
    if (F.isDeclaration())
      continue;
    // Find all alloca of structs inside the function body
    for (auto &BB : F) {
      for (auto &I : BB) {
        if (I.getOpcode() == Instruction::Alloca) {
          auto *type = I.getType()->getPointerElementType();
          if (type->isStructTy()) {
            // Identified I is an alloca of a struct
            DEBUG_WITH_TYPE(DEBUG_TYPE_IR,
                            dbgs() << "Found an alloca of a struct: " << I
                                   << "\n");
            auto *structInfoPtr =
                StructManager->createOrGetStructFieldAccessInfo(
                    type, StructFieldAccessManager::StructDefinitionType::
                              SDT_LocalStruct);
            structInfoPtr->analyzeUsersOfStructValue(&I);
          } else if (type->isPointerTy() &&
                     type->getPointerElementType()->isStructTy()) {
            DEBUG_WITH_TYPE(DEBUG_TYPE_IR,
                            dbgs() << "Found an alloca of a struct*: " << I
                                   << "\n");
            auto *structInfoPtr =
                StructManager->createOrGetStructFieldAccessInfo(
                    type->getPointerElementType(),
                    StructFieldAccessManager::StructDefinitionType::
                        SDT_LocalStructPtr);
            structInfoPtr->analyzeUsersOfStructPointerValue(&I);
          } else if (type->isPointerTy() &&
                     type->getPointerElementType()->isPointerTy() &&
                     type->getPointerElementType()
                         ->getPointerElementType()
                         ->isStructTy()) {
            DEBUG_WITH_TYPE(DEBUG_TYPE_IR,
                            dbgs() << "Found an alloca of a struct**: " << I
                                   << " but we ignore this\n");
            StructManager->addStats(
                StructFieldAccessManager::DebugStats::DS_StructPtrPtr);
          }
        }
      }
    }
  }
  // Find uses of structs through function calls
  for (auto &F : M) {
    if (F.isDeclaration())
      continue;
    for (auto &AG : F.args()) {
      if (AG.getType()->isStructTy()) {
        DEBUG_WITH_TYPE(DEBUG_TYPE_IR,
                        dbgs()
                            << "Found an argument of a struct pass by value: "
                            << AG << " and no support for this yet\n");
        StructManager->addStats(
            StructFieldAccessManager::DebugStats::DS_FuncArgValue);
      }
      if (AG.getType()->isPointerTy() &&
          AG.getType()->getPointerElementType()->isStructTy()) {
        // Identified AG is an argument with a struct type
        auto *StructPtr = StructManager->getStructFieldAccessInfo(
            AG.getType()->getPointerElementType());
        if (StructPtr) {
          DEBUG_WITH_TYPE(
              DEBUG_TYPE_IR,
              dbgs() << "Found an argument of a struct defined in the module: "
                     << AG << "\n");
          StructPtr->analyzeUsersOfStructValue(&AG);
        } else {
          DEBUG_WITH_TYPE(DEBUG_TYPE_IR, dbgs()
                                             << "Found an argument of a struct "
                                                "not defined in the program: "
                                             << AG << "\n");
          StructManager->addStats(
              StructFieldAccessManager::DebugStats::DS_FuncArgNotDefined);
        }
      }
    }
  }
  // Summarizes all uses of fields in function calls
  DEBUG_WITH_TYPE(DEBUG_TYPE_IR, StructManager->debugPrintAllStructAccesses());
  DEBUG_WITH_TYPE(DEBUG_TYPE_IR, StructManager->debugPrintAnnotatedModule());
}

static void applyFilters(StructFieldAccessManager *StructManager) {
  StructManager->summarizeFunctionCalls();
  StructManager->applyFiltersToStructs();
  StructManager->printStats();
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
  if (!PerformCodeAnalysisOnly) {
    StructManager.buildCloseProximityRelations();
    // DEBUG(StructManager.debugPrintAllCPGs());
    StructManager.debugPrintAllCPGs();
  }
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
