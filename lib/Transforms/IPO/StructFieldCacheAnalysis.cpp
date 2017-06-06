#include "llvm/Transforms/IPO/StructFieldCacheAnalysis.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/EquivalenceClasses.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalObject.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Operator.h"
#include "llvm/Pass.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Analysis/BlockFrequencyInfo.h"
#include "llvm/Analysis/BlockFrequencyInfoImpl.h"
#include "llvm/Analysis/BranchProbabilityInfo.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/IR/CFG.h"
#include <unordered_map>
#include <vector>

using namespace llvm;

#define DEBUG_TYPE "struct-analysis"

namespace{

class StructFieldCacheAnalysisPass : public ModulePass {
 public:
  static char ID;
  StructFieldCacheAnalysisPass() : ModulePass(ID) {
    initializeStructFieldCacheAnalysisPassPass(*PassRegistry::getPassRegistry());
  };
  StructFieldCacheAnalysisPass(const std::string& path) : ModulePass(ID), ProfileFileName(path) {
    initializeStructFieldCacheAnalysisPassPass(*PassRegistry::getPassRegistry());
  };
 private:
  StringRef ProfileFileName;
  bool runOnModule(Module &M) override;
  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.addRequired<BlockFrequencyInfoWrapperPass>();
  }

};
}

char StructFieldCacheAnalysisPass::ID = 0;
INITIALIZE_PASS_BEGIN(StructFieldCacheAnalysisPass, "struct-field-cache-analysis", "Struct Field Cache Analysis", false, false)
INITIALIZE_PASS_END(StructFieldCacheAnalysisPass, "struct-field-cache-analysis", "Struct Field Cahce Analysis", false, false)
ModulePass *llvm::createStructFieldCacheAnalysisPass(const std::string& path) { return new StructFieldCacheAnalysisPass(path); }

static bool performStructFieldCacheAnalysis(Module &M, StringRef ProfileFileName)
{
  printf("Dummy output of struct field cache analysis, reading from %s\n", ProfileFileName.str().c_str());
  return true;
}

StructFieldCacheAnalysis::StructFieldCacheAnalysis(std::string Filename): ProfileFileName(std::move(Filename)) {}

PreservedAnalyses StructFieldCacheAnalysis::run(Module &M, ModuleAnalysisManager &AM) {
  if (!performStructFieldCacheAnalysis(M, ProfileFileName))
    return PreservedAnalyses::all();
  return PreservedAnalyses::none();
}


bool StructFieldCacheAnalysisPass::runOnModule(Module &M){
  return performStructFieldCacheAnalysis(M, ProfileFileName);
}
