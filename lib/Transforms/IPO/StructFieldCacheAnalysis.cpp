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
// This pass performs analysis on cache-aware structure field accesses based on
// the following paper and reports recommendations on changes to make on the
// source code to improve performance.
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

static cl::opt<unsigned>
MinimalAccessCountForAnalysis("struct-analysis-minimal-count", cl::init(1), cl::Hidden,
                              cl::desc("Minimal access count to make the struct eligible for analysis"));

// Functions for StructFieldAccessManager
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
        new StructFieldAccessInfo(ST, SType, CurrentModule, this, NULL);
    return def;
  }
}

StructFieldAccessInfo *
StructFieldAccessManager::getStructFieldAccessInfo(const Type *T) const {
  if (!isa<StructType>(T))
    return NULL;
  auto *ST = cast<StructType>(T);
  auto ret = StructFieldAccessInfoMap.find(ST);
  if (ret != StructFieldAccessInfoMap.end())
    return ret->second;
  else
    return NULL;
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

void StructFieldAccessManager::summarizeFunctionCalls()
{
  for (auto &it : StructFieldAccessInfoMap){
    it.second->summarizeFunctionCalls();
  }
}

void StructFieldAccessManager::applyFiltersToStructs()
{
  // TODO: This function needs more work to add more filters to reduce the number of structs for analysis
  DEBUG_WITH_TYPE(DEBUG_TYPE_IR, dbgs() << "To apply filters to structs\n");
  for (auto it = StructFieldAccessInfoMap.begin(); it != StructFieldAccessInfoMap.end(); ){
    if (!it->second->isEligible()){
      delete it->second;
      auto ToRemove = it++;
      StructFieldAccessInfoMap.erase(ToRemove);
      addStats(DebugStats::DS_PassedIntoOutsideFunction);
    }
    else if (it->second->getTotalNumFieldAccess() < MinimalAccessCountForAnalysis){
      delete it->second;
      auto ToRemove = it++;
      StructFieldAccessInfoMap.erase(ToRemove);
      addStats(DebugStats::DS_NoAccess);
    }
    else{
      HotnessAnalyzer->addStruct(it->second);
      it++;
    }
  }
  DEBUG_WITH_TYPE(DEBUG_TYPE_STATS, HotnessAnalyzer->generateHistogram());
  for (auto it = StructFieldAccessInfoMap.begin(); it != StructFieldAccessInfoMap.end(); ){
    if (!HotnessAnalyzer->isHot(it->second)){
      delete it->second;
      auto ToRemove = it++;
      StructFieldAccessInfoMap.erase(ToRemove);
      addStats(DebugStats::DS_FilterColdStructs);
    }
    else{
      it++;
    }
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
      outs() << "A literal struct defined as " << StructDefinitionTypeNames[it.second->getStructDefinition()] << " has " << Result << " accesses and " << Hotness <<" execution count.\n";
      FILE_OS << "Literal," << Result << "\n";
    }
    else{
      outs() << "Struct [" << type->getStructName() << "] defined as " << StructDefinitionTypeNames[it.second->getStructDefinition()] << " has " << Result << " accesses and " << Hotness << " execution count.\n";
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

static bool performStructFieldCacheAnalysis(
    Module &M, function_ref<BlockFrequencyInfo *(Function &)> LookupBFI) {
  DEBUG(dbgs() << "Start of struct field cache analysis\n");
  StructFieldAccessManager StructManager(M, LookupBFI);
  // Step 0 - retrieve debug info for all struct TODO: disable for now because
  // it's not supporting annonymous structs
  // StructManager.retrieveDebugInfoForAllStructs();
  // Step 1 - perform IR analysis to collect info of all structs
  performIRAnalysis(M, &StructManager);
  StructManager.summarizeFunctionCalls();
  StructManager.applyFiltersToStructs();
  StructManager.printStats();
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
  if (!performStructFieldCacheAnalysis(M, LookupBFI))
    return PreservedAnalyses::all();
  return PreservedAnalyses::none();
}

bool StructFieldCacheAnalysisPass::runOnModule(Module &M) {
  auto LookupBFI = [this](Function &F) {
    return &this->getAnalysis<BlockFrequencyInfoWrapperPass>(F).getBFI();
  };
  return performStructFieldCacheAnalysis(M, LookupBFI);
}
