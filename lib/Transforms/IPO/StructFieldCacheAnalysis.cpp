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
