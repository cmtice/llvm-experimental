// lib/Tranforms/IPO/StructFieldCacheAnalysis.cpp - Performs Cache-Aware Structure Analysis-*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===--------------------------------------------------------------------------------------------------===//
//
// This pass performs analysis on cache-aware structure field accesses based on the following paper
// and reports recommendations on changes to make on the source code to improve performance.
//  [1] M. Hagog, C. Tice “Cache Aware Data Layout Reorganization Optimization in GCC”, Proceedings
//      of the GCC Developers’ Summit,  Ottawa, 2005.
//
//===--------------------------------------------------------------------------------------------------===//

#include "llvm/Transforms/IPO/StructFieldCacheAnalysis.h"
#include "llvm/Pass.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Analysis/BlockFrequencyInfo.h"
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/IR/AssemblyAnnotationWriter.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/FormattedStream.h"
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
class StructInfo
{
  /* This class is used to store all access information for each struct declared in
    the program. It records all loads and stores to all fields of the struct to provide
    essential information for cache-aware struct field analysis.
  */
 public:
  StructInfo(const Type* T, GlobalProfileInfo* P): StructType(T), NumElements(T->getStructNumElements()), ProfData(P) {}
  // Record all users of a GEP instruction that calculates the address of a field.
  // It's the only supported way to add access to a field for now
  void addAccessFromGEP(const Instruction* I);

  StringRef getStructName() const { return StructType->getStructName(); }
 private:
  const Type* StructType;
  unsigned NumElements;
  GlobalProfileInfo* ProfData;

  // Private functions
  // Calculate which field of the struct is the GEP pointing to
  unsigned calculateFieldNumFromGEP(const Instruction* I) const;

};
class AllStructInfo
{
  /* This class is used to keep track of all StructInfo objects in the program
     and make sure only one StructInfo object for each type of struct declared
     in the program.
  */
 public:
  AllStructInfo(GlobalProfileInfo* P): ProfData(P) {};
  // Check if the struct type is created before; if not, create a new StructInfo object for it
  StructInfo* createOrGetStructInfo(const Type* T);
  // Retrieve the pointer to the previous created StructInfo object for the type
  StructInfo* getStructInfo(const Type* T);
}

class GlobalProfileInfo
{
 public:
  GlobalProfileInfo(const Module& M): CurrentModule(M), OS(1, false, true) {}
  // Add profile information of a function
  void addFunction(const Function& F, BlockFrequencyInfo* BFI);
  // Get execution count for a Basicblock after all profile data are collected
  Optional<uint64_t> getBBCount(const BasicBlock* BB) const;
  // Debug print module
  void printModule() {
    OS << CurrentModule << '\n';
  }
  // Debug print annotated module after all profile data are collected
  void printAnnotatedModule();
 private:
  const Module& CurrentModule;
  raw_fd_ostream OS;
  std::unordered_map<const Function*, uint64_t> CountsPerFunc;
  std::unordered_map<const BasicBlock*, uint64_t> CountsPerBB;
};
class StructFieldCacheAnalysisAnnotatedWriter : public AssemblyAnnotationWriter {
 public:
  StructFieldCacheAnalysisAnnotatedWriter(const GlobalProfileInfo* P): Profile(P) {}

  // Override the base class function to print an annotate message after each basic block
  virtual void emitBasicBlockEndAnnot(const BasicBlock* BB,
                                      formatted_raw_ostream &OS) {
    auto count = profile->getBBCount(BB);
    if (count.hasValue()){
      OS.changeColor(raw_ostream::YELLOW, false, false);
      OS << "; [prof count = " << count.getValue() << "]\n";
      OS.resetColor();
    }
    else{
      OS.changeColor(raw_ostream::YELLOW, false, false);
      OS << "; [prof count not found " << "]\n";
      OS.resetColor();
    }
  }

  /* TODO: implement this after struct access are found
  virtual void emitInstructionAnnot(const Instruction *I,
                                    formatted_raw_ostream &OS) {
  }
  */
 private:
  const GlobalProfileInfo* Profile;
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

Optional<uint64_t> GlobalProfileInfo::getBBCount(const BasicBlock* BB) const
{
  Optional<uint64_t> ret;
  auto it = CountsPerBB.find(BB);
  if (it != CountsPerBB.end()){
    ret = it->second;
  }
  return ret;
}

void GlobalProfileInfo::printAnnotatedModule()
{
  StructFieldCacheAnalysisAnnotatedWriter Writer(this);
  OS.changeColor(raw_ostream::YELLOW);
  OS << "Annotated module print\n";
  OS.resetColor();
  CurrentModule.print(OS, &Writer);
  OS.resetColor();
}

static void collectAllStructAccess(Module &M, GlobalProfileInfo* profData)
{
  AllStructInfo allStructs(profData);
  for (auto &F : M){
    if (F.isDeclaration())
      continue;
    //Find all alloca of structs inside the function body
    for (auto &BB : F){
      for (auto &I: BB){
        if (I.getOpcode() == Instruction::Alloca){
          assert(I->getType()->isPointerTy());
          auto* type = I->getType()->getPointerElementType();
          if (type->isStructType()){
            auto* structInfoPtr = allStructs.createOrGetStructInfo(type);
          }
        }
      }
    }
  }
}

static bool performStructFieldCacheAnalysis(Module &M,
                                            StringRef ProfileFileName,
                                            function_ref<BlockFrequencyInfo *(Function &)> LookupBFI)
{
  // printf("Dummy output from StructFieldCacheAnalysis\n");
  GlobalProfileInfo allProfiles(M);
  for (auto &F : M){
    if (F.isDeclaration())
      continue;
    auto* BFI = LookupBFI(F);
    allProfiles.addFunction(F, BFI);
  }
  allProfiles.printAnnotatedModule();
  // perform IR analysis to collect info of all structs

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
