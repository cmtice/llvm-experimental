// llvm/Tranforms/IPO/StructFieldCacheAnalysis.h - Performs Cache-Aware Structure Analysis- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------------------------------===//
//
// This pass performs analysis on cache-aware structure field accesses based on the following paper
// and reports recommendations on changes to make on the source code to improve performance.
//  [1] M. Hagog, C. Tice “Cache Aware Data Layout Reorganization Optimization in GCC”, Proceedings
//      of the GCC Developers’ Summit,  Ottawa, 2005.
//
//===----------------------------------------------------------------------------------------------===//

#ifndef LLVM_TRANSFORMS_IPO_STRUCTFIELDCACHEANALYSIS_H
#define LLVM_TRANSFORMS_IPO_STRUCTFIELDCACHEANALYSIS_H

#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"

namespace llvm {
class StructFieldCacheAnalysis : public PassInfoMixin<StructFieldCacheAnalysis> {
public:
  PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM);
  StructFieldCacheAnalysis();
};
}
#endif // LLVM_TRANSFORMS_IPO_STRUCTFIELDCACHEANALYSIS_H
