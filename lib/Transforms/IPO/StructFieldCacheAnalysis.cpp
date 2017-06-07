// lib/Tranforms/IPO/StructFieldCacheAnalysis.cpp - Performs Cache-Aware Structure Analysis- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===-----------------------------------------------------------------------------------------------===//
//
// This pass performs analysis on cache-aware structure field accesses based on the following paper
// and reports recommendations on changes to make on the source code to improve performance.
//  [1] M. Hagog, C. Tice “Cache Aware Data Layout Reorganization Optimization in GCC”, Proceedings
//      of the GCC Developers’ Summit,  Ottawa, 2005.
//
//===-----------------------------------------------------------------------------------------------===//

#include "llvm/Transforms/IPO/StructFieldCacheAnalysis.h"
#include "llvm/Pass.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Analysis/BlockFrequencyInfo.h"
#include "llvm/IR/DiagnosticInfo.h"
#include <unordered_map>
#include <vector>

/*#include "llvm/ADT/DenseSet.h"
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
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Analysis/BlockFrequencyInfo.h"
#include "llvm/Analysis/BlockFrequencyInfoImpl.h"
#include "llvm/Analysis/BranchProbabilityInfo.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/IR/CFG.h"
*/

using namespace llvm;

#define DEBUG_TYPE "struct-field-cache-analysis"

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
INITIALIZE_PASS_DEPENDENCY(BlockFrequencyInfoWrapperPass)
INITIALIZE_PASS_END(StructFieldCacheAnalysisPass, "struct-field-cache-analysis", "Struct Field Cahce Analysis", false, false)
ModulePass *llvm::createStructFieldCacheAnalysisPass(const std::string& path) { return new StructFieldCacheAnalysisPass(path); }

namespace llvm{
class StructFieldCacheAnalysisAnnotatedWriter : public AssemblyAnnotationWriter {
  const GlobalProfileInfo* profile;

 public:
  StructFieldCacheAnalysisAnnotatedWriter(const GlobalProfileInfo* P): profile(P) {}

  virtual void emitBasicBlockEndAnnot(const BasicBlock* BB,
                                      formatted_raw_ostream &OS) {
    auto count = profile->getBBCount(BB);
    if (count){
      OS.changeColor(raw_ostream::YELLOW);
      OS << "; prof count = " << count << "\n";
      OS.resetColor();
    }
  }

  /* TODO: implement this after struct access are found
  virtual void emitInstructionAnnot(const Instruction *I,
                                    formatted_raw_ostream &OS) {
  }
  */
};

class GlobalProfileInfo
{
 public:
  GlobalProfileInfo(const Module& M): module(M), context(M.getContext()), OS(1, false, true) {}
  void addFunction(const Function& F, BlockFrequencyInfo* BFI);
  uint64_t getBBCount(const BasicBlock* BB);
  void printModule() {
    OS << module << '\n';
  }
  void printAnnotatedModule();
 private:
  const Module& module;
  const LLVMContext& context;
  raw_fd_ostream OS;
  std::unordered_map<const Function*, uint64_t> CountsPerFunc;
  std::unordered_map<const BasicBlock*, uint64_t> CountsPerBB;
};
}

void GlobalProfileInfo::addFunction(const Function& F, BlockFrequencyInfo* BFI)
{
  if (!F.getEntryCount().hasValue()){
    OS.changeColor(raw_ostream::YELLOW);
    OS << "Warning: Function " << F.getName() << " doesn't have entry count\n";
    OS.resetColor();
    return;
  }
  OS << "Adding function " << F.getName() << " has entry count: " << F.getEntryCount().getValue() << "\n";
  BFI->print(OS);
  for (auto &B : F){
    assert(CountsPerBB.find(&B) == CountsPerBB.end());
    auto count = BFI->getBlockProfileCount(&B);
    if (count){
      CountsPerBB[&B] = count.getValue();
    }
    else{
      OS.changeColor(raw_ostream::YELLOW);
      OS << "Warning: BB (@" << &B << ") ";
      OS.resetColor();
      OS << B << "\n";
      OS.changeColor(raw_ostream::YELLOW);
      OS << "has no execution count in profile\n";
      OS.resetColor();
    }
  }
}

Optional<uint64_t> GlobalProfileInfo::getBBCount(const BasicBlock* BB)
{
  Optional<uint64_t> ret;
  assert(CountsPerBB.find(BB) != CountsPerBB.end());
  return CountsPerBB[BB];
}

void GlobalProfileInfo::printAnnotatedModule()
{
  StructFieldCacheAnalysisAnnotatedWriter writer(this);
  OS.changeColor(raw_ostream::YELLOW);
  OS << "Annotated module print\n";
  module.print(OS, writer);
  OS.resetColor();
}

static bool performStructFieldCacheAnalysis(Module &M,
                                            StringRef ProfileFileName,
                                            function_ref<BlockFrequencyInfo *(Function &)> LookupBFI)
{
  //printf("Dummy output from StructFieldCacheAnalysis\n");
  GlobalProfileInfo allProfiles(M);
  // retrieve profile data
  allProfiles.printModule();
  for (auto &F : M){
    if (F.isDeclaration())
      continue;
    auto* BFI = LookupBFI(F);
    allProfiles.addFunction(F, BFI);
  }
  allProfiles.printAnnotatedModule();
  return true;
}

StructFieldCacheAnalysis::StructFieldCacheAnalysis(std::string Filename): ProfileFileName(std::move(Filename)) {}

PreservedAnalyses StructFieldCacheAnalysis::run(Module &M, ModuleAnalysisManager &AM) {
  auto &FAM = AM.getResult<FunctionAnalysisManagerModuleProxy>(M).getManager();
  auto LookupBFI = [&FAM](Function &F) {
    return &FAM.getResult<BlockFrequencyAnalysis>(F);
  };
  if (!performStructFieldCacheAnalysis(M, ProfileFileName, LookupBFI))
    return PreservedAnalyses::all();
  return PreservedAnalyses::none();
}


bool StructFieldCacheAnalysisPass::runOnModule(Module &M){
  auto LookupBFI = [this](Function &F) {
    return &this->getAnalysis<BlockFrequencyInfoWrapperPass>(F).getBFI();
  };
  return performStructFieldCacheAnalysis(M, ProfileFileName, LookupBFI);
}
