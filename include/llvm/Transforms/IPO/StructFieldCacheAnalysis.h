//===-- StructFieldCacheAnalysis.h - Performs Cache Aware Structure Analysis- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This pass exports all llvm.bitset's found in the module in the form of a
// __cfi_check function, which can be used to verify cross-DSO call targets.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TRANSFORMS_IPO_STRUCTFIELDCACHEANALYSIS_H
#define LLVM_TRANSFORMS_IPO_STRUCTFIELDCACHEANALYSIS_H

#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"

namespace llvm {
class StructFieldCacheAnalysis : public PassInfoMixin<StructFieldCacheAnalysis> {
public:
  PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM);
  StructFieldCacheAnalysis(std::string FileName = "");
 private:
  std::string ProfileFileName;
};
}
#endif // LLVM_TRANSFORMS_IPO_STRUCTFIELDCACHEANALYSIS_H
