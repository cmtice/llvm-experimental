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
#include "llvm/Analysis/BlockFrequencyInfo.h"
#include "llvm/IR/AssemblyAnnotationWriter.h"
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/Pass.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/IR/Instructions.h"
#include <unordered_map>
#include <vector>

using namespace llvm;

#define DEBUG_TYPE "struct-analysis"

// TODO: Debug flags, need to remove for final version
#define PERFORM_PROFILE 1
#define PERFORM_IR_ANALYSIS 1
#define PRINT_ANNOTATE_MODULE_AFTER_PROFILE 0
#define PRINT_ANNOTATE_MODULE_AFTER_IR_ANALYSIS 1
#define PRINT_ALL_ACCESSES 0
#define PRINT_STATS 1

namespace{
class StructFieldCacheAnalysisPass : public ModulePass {
 public:
  static char ID;
  StructFieldCacheAnalysisPass() : ModulePass(ID) {
    initializeStructFieldCacheAnalysisPassPass(*PassRegistry::getPassRegistry());
  }
 private:
  bool runOnModule(Module &M) override;
  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.addRequired<BlockFrequencyInfoWrapperPass>();
  }
};
}

char StructFieldCacheAnalysisPass::ID = 0;
INITIALIZE_PASS_BEGIN(StructFieldCacheAnalysisPass, "struct-field-cache-analysis", "Struct Field Cache Analysis", false, false)
INITIALIZE_PASS_DEPENDENCY(BlockFrequencyInfoWrapperPass)
INITIALIZE_PASS_END(StructFieldCacheAnalysisPass, "struct-field-cache-analysis", "Struct Field Cache Analysis", false, false)
ModulePass *llvm::createStructFieldCacheAnalysisPass() { return new StructFieldCacheAnalysisPass; }

namespace llvm{
class GlobalProfileInfo;
class StructInfo
{
  /* This class is used to store all access information for each struct declared in
    the program. It records all loads and stores to all fields of the struct to provide
    essential information for cache-aware struct field analysis.
  */
 public:
  StructInfo(const Type* T, const GlobalProfileInfo* P): StructureType(dyn_cast<StructType>(T)), NumElements(StructureType->getNumElements()), ProfData(P), OS(1, false, true) {}
  // Analyze a value pointing to a struct and collect struct access from it. It can be allocas or function args
  void analyzeUsersOfStructValue(const Value* V);
  // Obtain which field the instruction is accessing and return no val if not accessing any struct field
  Optional<unsigned> getAccessPattern(const Instruction* I) const;
  // Obtain total number of instructions that access the struct fields
  unsigned getNumAccessPatterns() const { return AccessPatternMap.size(); }
  // Print all instructions that access any struct field
  void printAllAccesses(raw_ostream& OS);
 private:
  const StructType* StructureType;
  unsigned NumElements;
  const GlobalProfileInfo* ProfData;
  // output stream
  raw_fd_ostream OS;
  //A map records all instructions accessing which field of the structure
  std::unordered_map<const Instruction*, unsigned> AccessPatternMap;

  // Private functions
  // Calculate which field of the struct is the GEP pointing to
  unsigned calculateFieldNumFromGEP(const Instruction* I) const;
  // Record all users of a GEP instruction that calculates the address of a field.
  // It's the only supported way to add access to a field for now
  void addAccessFromGEP(const Instruction* I);
  // Record an access pattern in the data structure
  void addAccessPattern(const Instruction* I, unsigned FieldNum);
};
class AllStructInfo
{
  /* This class is used to keep track of all StructInfo objects in the program
     and make sure only one StructInfo object for each type of struct declared
     in the program.
  */
 public:
  AllStructInfo(const Module& M, const GlobalProfileInfo* P): CurrentModule(M), ProfData(P), OS(1, false, true) {};
  // Check if the struct type is created before; if not, create a new StructInfo object for it
  StructInfo* createOrGetStructInfo(const Type* T);
  // Retrieve the pointer to the previous created StructInfo object for the type
  StructInfo* getStructInfo(const Type* T) const;
  // Retrive a pair of information if the instruction is accessing any struct type and field number
  Optional<std::pair<const Type*, unsigned> > getAccessStructInfo(const Instruction* I) const;
  // Print all accesses of all struct types defined in the program
  void printAllStructAccesses();
  // Print the IR of the module with annotated information about struct access
  void printAnnotatedModule();
  // Print a brief stats of struct access
  void printStats();
 private:
  const Module& CurrentModule;
  // Const pointer to GlobalProfileInfo for retrieving profile info
  const GlobalProfileInfo* ProfData;
  // output stream
  raw_fd_ostream OS;
  std::unordered_map<const Type*, StructInfo*> StructMap;
};

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
  // output stream
  raw_fd_ostream OS;
  std::unordered_map<const Function*, uint64_t> CountsPerFunc;
  std::unordered_map<const BasicBlock*, uint64_t> CountsPerBB;
};
class StructFieldCacheAnalysisAnnotatedWriter : public AssemblyAnnotationWriter {
 public:
  StructFieldCacheAnalysisAnnotatedWriter(const GlobalProfileInfo* P = NULL, const AllStructInfo* S = NULL): Profile(P), StructInfo(S) {}

  // Override the base class function to print an annotate message after each basic block
  virtual void emitBasicBlockEndAnnot(const BasicBlock* BB,
                                      formatted_raw_ostream &OS) {
    OS.resetColor();
    if (Profile == NULL)
      return;
    auto count = Profile->getBBCount(BB);
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

  virtual void emitInstructionAnnot(const Instruction *I,
                                    formatted_raw_ostream &OS) {
    if (StructInfo == NULL)
      return;
    if (auto pair = StructInfo->getAccessStructInfo(I)){
      OS.changeColor(raw_ostream::GREEN, false, false);
      OS << "; [Instruction is memory access on field " << pair.getValue().second << " of struct " << pair.getValue().first->getStructName() << "] ";
    }
    else{
      OS.resetColor();
    }
  }
 private:
  const GlobalProfileInfo* Profile;
  const AllStructInfo* StructInfo;
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
  StructFieldCacheAnalysisAnnotatedWriter Writer(this, NULL);
  std::error_code EC;
  raw_fd_ostream FILE_OS("AnnotatedModule.prof.ll", EC, llvm::sys::fs::F_RW);
  FILE_OS.changeColor(raw_ostream::YELLOW);
  FILE_OS << "Annotated module print\n";
  FILE_OS.resetColor();
  CurrentModule.print(FILE_OS, &Writer);
  FILE_OS.resetColor();
}

void StructInfo::addAccessPattern(const Instruction* I, unsigned FieldNum)
{
  assert(I->getOpcode() == Instruction::Load || I->getOpcode() == Instruction::Store); // Only loads and stores
  assert(AccessPatternMap.find(I) == AccessPatternMap.end());
  // Make sure the field type matches the instruction type
  Type* Ty;
  if (I->getOpcode() == Instruction::Load){
    auto* Inst = dyn_cast<LoadInst>(I);
    Ty = Inst->getPointerOperand()->getType()->getPointerElementType();
  }
  else{
    auto* Inst = dyn_cast<StoreInst>(I);
    Ty = Inst->getPointerOperand()->getType()->getPointerElementType();
  }
  //OS << "Found a Load/Store [" << *I << "] access struct field [" << FieldNum << "]\n";
  //OS << "Field type: [" << *StructureType->getElementType(FieldNum) << "] vs instruction type: [" << *Ty << "]\n";
  assert(Ty == StructureType->getElementType(FieldNum));
  AccessPatternMap[I] = FieldNum;
}

Optional<unsigned> StructInfo::getAccessPattern(const Instruction* I) const
{
  Optional<unsigned> ret;
  auto it = AccessPatternMap.find(I);
  if (it != AccessPatternMap.end()){
    ret = it->second;
  }
  return ret;
}

unsigned StructInfo::calculateFieldNumFromGEP(const Instruction* I) const
{
  //Operand 0 should be a pointer to the struct
  auto* Inst = dyn_cast<GetElementPtrInst>(I);
  auto* Op = Inst->getOperand(0);
  // Make sure Operand 0 is a struct type and matches the current struct type of StructInfo
  assert(Op->getType()->isPointerTy() && Op->getType()->getPointerElementType()->isStructTy() && Op->getType()->getPointerElementType() == StructureType);
  assert(Inst->getNumIndices() >= 2); // GEP to calculate struct field needs at least 2 indices (operand 1 and 2)
  //Operand 1 should be first index to the struct, usually 0; if not 0, it's like goto an element of an array of structs
  Op = I->getOperand(1);
  //TODO: ignore this index for now because it's the same for an array of structs
  assert(Op->getType()->isIntegerTy());
  //Operand 2 should be the index to the field, and be a constant
  Op = I->getOperand(2);
  auto* Index = dyn_cast<Constant>(Op);
  auto Offset = (unsigned)Index->getUniqueInteger().getZExtValue();
  assert(Offset < NumElements);
  //TODO: ignore indices after this one. If there's indices, the field has to be an array or struct
  return Offset;
}

void StructInfo::addAccessFromGEP(const Instruction* I)
{
  auto FieldLoc = calculateFieldNumFromGEP(I);
  for (auto *U : I->users()){
    auto* Inst = dyn_cast<Instruction>(U);
    if (Inst->getOpcode() == Instruction::Load || Inst->getOpcode() == Instruction::Store){
      addAccessPattern(Inst, FieldLoc);
    }
    else{
      //TODO: ignore for now but if the user is a call or invoke, need to track that function also
    }
  }
}

void StructInfo::analyzeUsersOfStructValue(const Value* V)
{
  assert(V->getType()->isPointerTy() && V->getType()->getPointerElementType()->isStructTy());
  for (auto *U : V->users()){
    auto *Inst = dyn_cast<Instruction>(U);
    if (Inst->getOpcode() != Instruction::GetElementPtr){
      // Only support access struct through GEP for now
      continue;
    }
    addAccessFromGEP(Inst);
  }
}

void StructInfo::printAllAccesses(raw_ostream& OS)
{
  for (auto &it : AccessPatternMap){
    OS << "\tInstruction [" << *it.first << "] accesses field number [" << it.second << "]\n";
  }
}

StructInfo* AllStructInfo::createOrGetStructInfo(const Type* T)
{
  if (auto* def = getStructInfo(T))
    return def;
  else{
    def = StructMap[T] = new StructInfo(T, ProfData);
    return def;
  }
}

StructInfo* AllStructInfo::getStructInfo(const Type* T) const
{
  auto ret = StructMap.find(T);
  if (ret != StructMap.end())
    return ret->second;
  else
    return NULL;
}

Optional<std::pair<const Type*, unsigned> > AllStructInfo::getAccessStructInfo(const Instruction* I) const
{
  Optional<std::pair<const Type*, unsigned> > ret;
  for (auto &it : StructMap){
    if (auto FieldNum = it.second->getAccessPattern(I)){
      return std::pair<const Type*, unsigned>(it.first, FieldNum.getValue());
    }
  }
  return ret;
}

void AllStructInfo::printAllStructAccesses()
{
  OS << "------------ Printing all struct accesses: ---------------- \n";
  for (auto &it : StructMap){
    OS.changeColor(raw_ostream::YELLOW);
    OS << "Struct [" << it.first->getStructName() << "] has accesses: \n";
    OS.changeColor(raw_ostream::GREEN);
    it.second->printAllAccesses(OS);
    OS.resetColor();
  }
  OS << "----------------------------------------------------------- \n";
}

void AllStructInfo::printAnnotatedModule()
{
  StructFieldCacheAnalysisAnnotatedWriter Writer(ProfData, this);
  std::error_code EC;
  raw_fd_ostream FILE_OS("AnnotatedModule.IR.ll", EC, llvm::sys::fs::F_RW);
  FILE_OS.changeColor(raw_ostream::YELLOW);
  FILE_OS << "Annotated module print\n";
  FILE_OS.resetColor();
  CurrentModule.print(FILE_OS, &Writer);
  FILE_OS.resetColor();
}

void AllStructInfo::printStats()
{
  OS << "------------ Printing stats for struct accesses: ---------------- \n";
  OS.changeColor(raw_ostream::YELLOW);
  OS << "There are " << StructMap.size() << " struct types are accessed in the program\n";
  for (auto &it : StructMap){
    OS << "Struct [" << it.first->getStructName() << "] has " << it.second->getNumAccessPatterns() << " accesses.\n";
  }
  OS.resetColor();
  OS << "----------------------------------------------------------------- \n";
}

static void collectAllStructAccess(Module &M, GlobalProfileInfo* profData)
{
  AllStructInfo allStructs(M, profData);
  // Find all structs declared by allocas
  for (auto &F : M){
    if (F.isDeclaration())
      continue;
    //Find all alloca of structs inside the function body
    for (auto &BB : F){
      for (auto &I: BB){
        if (I.getOpcode() == Instruction::Alloca){
          assert(I.getType()->isPointerTy());
          auto* type = I.getType()->getPointerElementType();
          if (type->isStructTy()){
            // Identified I is an alloca of a struct
            auto* structInfoPtr = allStructs.createOrGetStructInfo(type);
            structInfoPtr->analyzeUsersOfStructValue(&I);
          }
        }
      }
    }
  }
  // Find all global structs
  for (auto &G : M.globals()){
    printf("Global variable:\n");
    G.dump();
    if (G.getType()->isPointerTy() && G.getType()->getPointerElementType()->isStructTy()){
      printf("Found a global has struct type\n");
      G.dump();
    }
  }
  //Find uses of structs through function calls
  for (auto &F : M){
    if (F.isDeclaration())
      continue;
    for (auto &AG : F.args()){
      if (AG.getType()->isPointerTy() && AG.getType()->getPointerElementType()->isStructTy()){
        // Identified AG is an argument with a struct type
        auto* StructPtr = allStructs.getStructInfo(AG.getType()->getPointerElementType());
        if (StructPtr){
          StructPtr->analyzeUsersOfStructValue(&AG);
        }
        else{
          // TODO: Remove this message after debugging
          printf("Struct type not defined in the program\n");
          AG.getType()->getPointerElementType()->dump();
        }
      }
    }
  }
#if PRINT_ALL_ACCESSES
  allStructs.printAllStructAccesses();
#endif
#if PRINT_ANNOTATE_MODULE_AFTER_IR_ANALYSIS
  allStructs.printAnnotatedModule();
#endif
#if PRINT_STATS
  allStructs.printStats();
#endif
}

static bool performStructFieldCacheAnalysis(Module &M,
                                            function_ref<BlockFrequencyInfo *(Function &)> LookupBFI)
{
  DEBUG_WITH_TYPE(DEBUG_TYPE, dbgs() << "Dummy output of struct field cache analysis\n");
#if PERFORM_PROFILE
  GlobalProfileInfo allProfiles(M);
  for (auto &F : M){
    if (F.isDeclaration())
      continue;
    auto* BFI = LookupBFI(F);
    allProfiles.addFunction(F, BFI);
  }
#if PRINT_ANNOTATE_MODULE_AFTER_PROFILE
  allProfiles.printAnnotatedModule();
#endif
#endif
  // perform IR analysis to collect info of all structs
#if PERFORM_IR_ANALYSIS
  collectAllStructAccess(M, &allProfiles);
#endif
  return false;
}

StructFieldCacheAnalysis::StructFieldCacheAnalysis() {}

PreservedAnalyses StructFieldCacheAnalysis::run(Module &M, ModuleAnalysisManager &AM) {
  auto &FAM = AM.getResult<FunctionAnalysisManagerModuleProxy>(M).getManager();
  auto LookupBFI = [&FAM](Function &F) {
    return &FAM.getResult<BlockFrequencyAnalysis>(F);
  };
  if (!performStructFieldCacheAnalysis(M, LookupBFI))
    return PreservedAnalyses::all();
  return PreservedAnalyses::none();
}


bool StructFieldCacheAnalysisPass::runOnModule(Module &M){
  auto LookupBFI = [this](Function &F) {
    return &this->getAnalysis<BlockFrequencyInfoWrapperPass>(F).getBFI();
  };
  return performStructFieldCacheAnalysis(M, LookupBFI);
}
