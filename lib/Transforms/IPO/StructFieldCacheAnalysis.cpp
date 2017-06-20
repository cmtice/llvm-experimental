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
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Operator.h"
#include "llvm/Pass.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/IPO.h"

#include <llvm/IR/DebugInfoMetadata.h>
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
class StructFieldAccessInfo;
class StructFieldAccessManager
{
  /* This class is used to keep track of all StructFieldAccessInfo objects in the program
     and make sure only one StructFieldAccessInfo object for each type of struct declared
     in the program.
  */
 public:
  StructFieldAccessManager(const Module& M, function_ref<BlockFrequencyInfo *(Function &)> L):
      CurrentModule(M), LookupBFI(L), StatCounts(stats::max_stats) {};
  // Retrieve debug info for all structs defined in the program
  void retrieveDebugInfoForAllStructs();
  // Check if the struct type is created before; if not, create a new StructFieldAccessInfo object for it
  StructFieldAccessInfo* createOrGetStructFieldAccessInfo(const Type* T, const Function* F);
  // Retrieve the pointer to the previous created StructFieldAccessInfo object for the type
  StructFieldAccessInfo* getStructFieldAccessInfo(const Type* T) const;
  // Retrieve execution count for a basic block
  Optional<uint64_t> getExecutionCount(const BasicBlock* BB) const{
    Function* func = const_cast<Function*>(BB->getParent());
    return LookupBFI(*func)->getBlockProfileCount(BB);
  }
  // Retrive a pair of information if the instruction is accessing any struct type and field number
  Optional<std::pair<const Type*, unsigned> > getFieldAccessOnInstruction(const Instruction* I) const;
  // Print all accesses of all struct types defined in the program
  void debugPrintAllStructAccesses();
  // Print the IR of the module with annotated information about struct access
  void debugPrintAnnotatedModule();

  // For stats
  enum stats{
    struct_pointer_pointer,
    func_arg_value,
    func_arg_not_defined,
    gep_in_arg,
    user_not_instruction_nor_operator,
    max_stats
  };
  // Increment stats for one category
  void addStats(unsigned Category) { StatCounts[Category]++; }
  // Print a brief stats of struct access
  void printStats();

 private:
  const Module& CurrentModule;
  // Function reference that is used to retrive execution count for basic block
  function_ref<BlockFrequencyInfo *(Function &)> LookupBFI;
  std::unordered_map<const Type*, StructFieldAccessInfo*> StructFieldAccessInfoMap;
  std::vector<unsigned> StatCounts;
  const std::vector<std::string> StatNames = {"Variable type is Struct**",
                                              "Function argument is a value",
                                              "Function argument is not defined in the program",
                                              "GEP value passed into function calls",
                                              "User is not Instruction nor Operator"
  };

  std::unordered_map<const StructType*, DICompositeType*> StructDebugInfo;
  // Retrieve debug info of the struct definition from the function
  DICompositeType* retrieveDebugInfoForStruct(const Type* T);
  void recursiveFindDebugInfoOfStruct(DIType* Ty);
};

class StructFieldAccessInfo
{
  /* This class is used to store all access information for each struct declared in
    the program. It records all loads and stores to all fields of the struct to provide
    essential information for cache-aware struct field analysis.
  */
 public:
  StructFieldAccessInfo(const Type* T, const Module& MD, const StructFieldAccessManager* M, const DICompositeType* D): CurrentModule(MD), StructureType(dyn_cast<StructType>(T)), DebugInfo(D), NumElements(StructureType->getNumElements()), StructManager(M), StatCounts(StructFieldAccessManager::stats::max_stats) {}
  // Analyze a value pointing to a struct and collect struct access from it. It can be allocas/function args/globals
  void analyzeUsersOfStructValue(const Value* V);
  // Analyze a value pointing to a struct* and collect struct access from it. It can be allocas/function args/globals
  void analyzeUsersOfStructPointerValue(const Value* V);
  // Obtain which field the instruction is accessing and return no val if not accessing any struct field
  Optional<unsigned> getAccessFieldNum(const Instruction* I) const;
  // Obtain total number of instructions that access the struct fields
  unsigned getTotalNumFieldAccess() const { return FieldAccessMap.size(); }
  // Obtain execution count for the BasicBlock/Instruction from profiling info, if any
  Optional<uint64_t> getExecutionCount(const BasicBlock* BB) const {
    return StructManager->getExecutionCount(BB);
  }
  Optional<uint64_t> getExecutionCount(const Instruction* I) const {
    return StructManager->getExecutionCount(I->getParent());
  }
  // Print all instructions that access any struct field
  void debugPrintAllStructAccesses(raw_ostream& OS);

  // For stats
  void addStats(unsigned Category) { StatCounts[Category]++; }
  unsigned getStats(unsigned Category) const { return StatCounts[Category]; }

 private:
  const Module& CurrentModule;
  const StructType* StructureType;
  const DICompositeType* DebugInfo;
  unsigned NumElements;
  const StructFieldAccessManager* StructManager;
  //A map records all instructions accessing which field of the structure
  std::unordered_map<const Instruction*, unsigned> FieldAccessMap;
  std::vector<unsigned> StatCounts;

  // Private functions
  // Calculate which field of the struct is the GEP pointing to, from GetElementPtrInst or GEPOperator
  unsigned calculateFieldNumFromGEP(const User* U) const;
  // Record all users of a GEP instruction/operator that calculates the address of a field.
  // It's the only supported way to add access to a field for now
  void addFieldAccessFromGEP(const User* U);
  // Record an access pattern in the data structure
  void addFieldAccessNum(const Instruction* I, unsigned FieldNum);
  // Use debug info to remaping struct fields (not using yet)
  void remapFieldFromDebugInfo();
};

class StructFieldCacheAnalysisAnnotatedWriter : public AssemblyAnnotationWriter {
 public:
  StructFieldCacheAnalysisAnnotatedWriter(const StructFieldAccessManager* S = NULL): StructManager(S) {}

  // Override the base class function to print an annotate message after each basic block
  virtual void emitBasicBlockEndAnnot(const BasicBlock* BB,
                                      formatted_raw_ostream &OS) {
    OS.resetColor();
    auto count = StructManager->getExecutionCount(BB);
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
    if (StructManager == NULL)
      return;
    if (auto pair = StructManager->getFieldAccessOnInstruction(I)){
      OS.changeColor(raw_ostream::GREEN, false, false);
      auto* type = pair.getValue().first;
      assert(isa<StructType>(type));
      if (dyn_cast<StructType>(type)->isLiteral())
        OS << "; [Instruction is memory access on field " << pair.getValue().second << " of a literal struct.] ";
      else
        OS << "; [Instruction is memory access on field " << pair.getValue().second << " of struct " << type->getStructName() << "] ";
    }
    else{
      OS.resetColor();
    }
  }
 private:
  const StructFieldAccessManager* StructManager;
};
}

// Functions for StructFieldAccessInfo
void StructFieldAccessInfo::remapFieldFromDebugInfo()
{
  if (DebugInfo){
    for (unsigned i = 0; i < DebugInfo->getElements().size(); i++){
      DINode* node = DebugInfo->getElements()[i];
      assert(node && isa<DIDerivedType>(node) && dyn_cast<DIDerivedType>(node)->getTag() == dwarf::Tag::DW_TAG_member);
      Metadata* T = dyn_cast<DIDerivedType>(node)->getBaseType();
      assert(T && isa<DIBasicType>(T));
      outs() << "Element " << i << ": " << dyn_cast<DIDerivedType>(node)->getName() << " type: " << dyn_cast<DIBasicType>(T)->getName() << "\n";
    }
  }
}
void StructFieldAccessInfo::addFieldAccessNum(const Instruction* I, unsigned FieldNum)
{
  assert(I->getOpcode() == Instruction::Load || I->getOpcode() == Instruction::Store); // Only loads and stores
  assert (FieldAccessMap.find(I) == FieldAccessMap.end());
  FieldAccessMap[I] = FieldNum;

  /*
  // Make sure the field type matches the instruction type
  Type* Ty;
  if (I->getOpcode() == Instruction::Load){
    assert(isa<LoadInst>(I));
    auto* Inst = dyn_cast<LoadInst>(I);
    Ty = Inst->getPointerOperand()->getType()->getPointerElementType();
  }
  else{
    assert(isa<StoreInst>(I));
    auto* Inst = dyn_cast<StoreInst>(I);
    Ty = Inst->getPointerOperand()->getType()->getPointerElementType();
  }
  assert(StructureType->getElementType(FieldNum)->isStructTy() || Ty == StructureType->getElementType(FieldNum)); // TODO: this assertion will fail if a field is a struct*/
}

Optional<unsigned> StructFieldAccessInfo::getAccessFieldNum(const Instruction* I) const
{
  Optional<unsigned> ret;
  auto it = FieldAccessMap.find(I);
  if (it != FieldAccessMap.end()){
    ret = it->second;
  }
  return ret;
}

unsigned StructFieldAccessInfo::calculateFieldNumFromGEP(const User* U) const
{
  DEBUG(dbgs() << "Calculating field number from GEP: " << *U << "\n");
  //Operand 0 should be a pointer to the struct
  assert(isa<GetElementPtrInst>(U) || isa<GEPOperator>(U));
  auto* Op = U->getOperand(0);
  // Make sure Operand 0 is a struct type and matches the current struct type of StructFieldAccessInfo
  assert(Op->getType()->isPointerTy() && Op->getType()->getPointerElementType()->isStructTy() && Op->getType()->getPointerElementType() == StructureType);
  if (U->getNumOperands() < 3) // GEP to calculate struct field needs at least 2 indices (operand 1 and 2)
    return 0;
  //Operand 1 should be first index to the struct, usually 0; if not 0, it's like goto an element of an array of structs
  Op = U->getOperand(1);
  //TODO: ignore this index for now because it's the same for an array of structs
  assert(Op->getType()->isIntegerTy());
  //Operand 2 should be the index to the field, and be a constant
  Op = U->getOperand(2);
  assert(isa<Constant>(Op));
  auto* Index = dyn_cast<Constant>(Op);
  auto Offset = (unsigned)Index->getUniqueInteger().getZExtValue();
  assert(Offset < NumElements);
  //TODO: ignore indices after this one. If there's indices, the field has to be an array or struct
  return Offset+1; // return field number starting from 1
}

void StructFieldAccessInfo::addFieldAccessFromGEP(const User* U)
{
  DEBUG(dbgs() << "Analyze all users of GEP: " << *U << "\n");
  assert(isa<GetElementPtrInst>(U) || isa<GEPOperator>(U));
  auto FieldLoc = calculateFieldNumFromGEP(U);
  if (FieldLoc == 0)
    return;
  for (auto *User : U->users()){
    DEBUG(dbgs() << "Check user of " << *U << ": " << *User << "\n");
    //DEBUG(dbgs() << "Print the use of this user: " << *User->getOperandList()->get() << " and its user: " << *User->getOperandList()->getUser() << "\n");
    //assert(isa<Instruction>(User) || isa<Operator>(User)); // || isa<Operator>(U));
    if (isa<Instruction>(User)){
      auto* Inst = dyn_cast<Instruction>(User);
      if (Inst->getOpcode() == Instruction::Load)
        addFieldAccessNum(Inst, FieldLoc);
      else if (Inst->getOpcode() == Instruction::Store){
        if (U == Inst->getOperand(1))
          addFieldAccessNum(Inst, FieldLoc);
      }
      else{
        if (Inst->getOpcode() == Instruction::Call || Inst->getOpcode() == Instruction::Invoke){
          addStats(StructFieldAccessManager::stats::gep_in_arg);
        }
      }
    }
    else if (isa<Operator>(User)){
      auto* Inst = dyn_cast<Operator>(U);
      assert (Inst->getOpcode() != Instruction::Load || Inst->getOpcode() != Instruction::Store
              || Inst->getOpcode() != Instruction::Call || Inst->getOpcode() != Instruction::Invoke);
    }
    else{
      addStats(StructFieldAccessManager::stats::user_not_instruction_nor_operator);
    }
  }
}

void StructFieldAccessInfo::analyzeUsersOfStructValue(const Value* V)
{
  assert(V->getType()->isPointerTy() && V->getType()->getPointerElementType()->isStructTy());
  for (auto *U : V->users()){
    DEBUG(dbgs() << "Analyzing user of " << *V << ": " << *U << "\n");
    //assert(isa<Instruction>(U) || isa<Operator>(U));
    if (isa<Instruction>(U)){
      auto *Inst = dyn_cast<Instruction>(U);
      if (Inst->getOpcode() != Instruction::GetElementPtr){
        // Only support access struct through GEP for now
        continue;
      }
      addFieldAccessFromGEP(Inst);
    }
    else if (isa<Operator>(U)){
      auto *Inst = dyn_cast<Operator>(U);
      if (Inst->getOpcode() != Instruction::GetElementPtr){
        // Only support access struct through GEP for now
        continue;
      }
      addFieldAccessFromGEP(Inst);
    }
    else{
      addStats(StructFieldAccessManager::stats::user_not_instruction_nor_operator);
    }
  }
}

void StructFieldAccessInfo::analyzeUsersOfStructPointerValue(const Value* V)
{
  // Analyze users of value defined as struct*
  assert(V->getType()->isPointerTy() && V->getType()->getPointerElementType()->isPointerTy() && V->getType()->getPointerElementType()->getPointerElementType()->isStructTy());
  for (auto *U : V->users()){
    DEBUG(dbgs() << "Analyzing user of " << *V << ": " << *U << "\n");
    //assert(isa<Instruction>(U) || isa<Operator>(U));
    if (isa<Instruction>(U)){
      auto *Inst = dyn_cast<Instruction>(U);
      if (Inst->getOpcode() != Instruction::Load){
        // Only load is allowed to access struct*
        continue;
      }
      analyzeUsersOfStructValue(Inst);
    }
    else if (isa<Operator>(U)){
      auto *Inst = dyn_cast<Operator>(U);
      if (Inst->getOpcode() != Instruction::Load){
        // Only support access struct through GEP for now
        continue;
      }
      analyzeUsersOfStructValue(Inst);
    }
    else{
      addStats(StructFieldAccessManager::stats::user_not_instruction_nor_operator);
    }
  }
}


void StructFieldAccessInfo::debugPrintAllStructAccesses(raw_ostream& OS)
{
  for (auto &it : FieldAccessMap){
    OS << "\tInstruction [" << *it.first << "] accesses field number [" << it.second << "]\n";
  }
}

// Functions for StructFieldAccessManager
void StructFieldAccessManager::recursiveFindDebugInfoOfStruct(DIType* Ty)
{
  while (Ty->getTag() == dwarf::Tag::DW_TAG_pointer_type ||
         Ty->getTag() == dwarf::Tag::DW_TAG_reference_type){
    assert(isa<DIDerivedType>(Ty));
    Metadata* MD = dyn_cast<DIDerivedType>(Ty)->getBaseType();
    if (MD == NULL)
      return;
    assert(isa<DIType>(MD));
    Ty = dyn_cast<DIType>(MD);
  }
  if (!isa<DICompositeType>(Ty))
    return;
  if (Ty->getTag() == dwarf::Tag::DW_TAG_structure_type ||
      Ty->getTag() == dwarf::Tag::DW_TAG_class_type){
    assert(isa<DICompositeType>(Ty));
    DEBUG(dbgs() << dyn_cast<DICompositeType>(Ty)->getName() << "\n");
    auto* ST = (Ty->getTag() == dwarf::Tag::DW_TAG_structure_type) ?
        CurrentModule.getTypeByName(StringRef("struct."+dyn_cast<DICompositeType>(Ty)->getName().str())) :
        CurrentModule.getTypeByName(StringRef("class."+dyn_cast<DICompositeType>(Ty)->getName().str()));
    assert(ST); // This assertion will fail on annonymous struct
    if (StructDebugInfo.find(ST) != StructDebugInfo.end()){
      DEBUG(dbgs() << "Found existing debugging info " << *dyn_cast<DICompositeType>(Ty));
      // FIXME: use the one with more struct fields, because they might be shown as two different debug info
      if (dyn_cast<DICompositeType>(Ty)->getElements().size() > StructDebugInfo[ST]->getElements().size())
        StructDebugInfo[ST] = dyn_cast<DICompositeType>(Ty);
    }
    else
      StructDebugInfo[ST] = dyn_cast<DICompositeType>(Ty);
  }
}

void StructFieldAccessManager::retrieveDebugInfoForAllStructs()
{
  for (auto& NMD : CurrentModule.named_metadata()){
    for (auto* it : NMD.operands()){
      if (isa<DICompileUnit>(it)){
        DEBUG(dbgs() << "Compile unit found" << *it << "\n");
        auto* MD = dyn_cast<DICompileUnit>(it);
        for (unsigned i = 0; i < MD->getRetainedTypes().size(); i++){
          if (Metadata* Ty = MD->getRetainedTypes()[i]){
            assert(isa<DIType>(Ty));
            recursiveFindDebugInfoOfStruct(dyn_cast<DIType>(Ty));
          }
        }
      }
    }
  }
  for (auto &F : CurrentModule){
    if (F.isDeclaration())
      continue;
    DEBUG(dbgs() << "Retrieving debug info from function: " << F.getName() << "\n");
    auto* SubProgram = F.getSubprogram();
    if (!SubProgram)
      continue;
    auto* SubroutineType = SubProgram->getType();
    assert(SubroutineType);
    DEBUG(dbgs() << *SubroutineType << " with size " << SubroutineType->getTypeArray().size() << "\n");
    for (unsigned i = 0; i < SubroutineType->getTypeArray().size(); i++){
      if (Metadata* Ty = SubroutineType->getTypeArray()[i]){
        assert(isa<DIType>(Ty));
        recursiveFindDebugInfoOfStruct(dyn_cast<DIType>(Ty));
      }
    }
  }
  for (auto &it : StructDebugInfo){
    DEBUG(dbgs() << "Struct " << it.first->getName() << " has got debug info: \n");
    auto* DebugInfo = it.second;
    for (unsigned i = 0; i < DebugInfo->getElements().size(); i++){
      DINode* node = DebugInfo->getElements()[i];
      if (node && isa<DIDerivedType>(node) && dyn_cast<DIDerivedType>(node)->getTag() == dwarf::Tag::DW_TAG_member){
        Metadata* T = dyn_cast<DIDerivedType>(node)->getBaseType();
        assert(T && isa<DIType>(T));
        if (!dyn_cast<DIType>(T)->getName().empty()){
          DEBUG(dbgs() << "Field " << i << ": " << dyn_cast<DIDerivedType>(node)->getName() << " type: " << dyn_cast<DIType>(T)->getName() << "\n");
        }
        else{
          if (dyn_cast<DIType>(T)->getTag() == dwarf::Tag::DW_TAG_structure_type){
            assert(isa<DICompositeType>(T));
            DEBUG(dbgs() << "Field " << i << ": " << dyn_cast<DIDerivedType>(node)->getName() << " type: " << "anonymous struct with " << dyn_cast<DICompositeType>(T)->getElements().size() << " fields\n");
          }
          else if (dyn_cast<DIType>(T)->getTag() == dwarf::Tag::DW_TAG_pointer_type){
            assert(isa<DIDerivedType>(T));
            Metadata* BT = dyn_cast<DIDerivedType>(T)->getBaseType();
            assert(BT && isa<DIType>(BT));
            if (!dyn_cast<DIType>(BT)->getName().empty()) // FIXME: only support one level pointer now
              DEBUG(dbgs() << "Field " << i << ": " << dyn_cast<DIDerivedType>(node)->getName() << " type: " << dyn_cast<DIType>(BT)->getName() << "*\n");
          }
        }
      }
    }
  }
}

DICompositeType* StructFieldAccessManager::retrieveDebugInfoForStruct(const Type* T)
{
  assert(isa<StructType>(T));
  auto* Ty = dyn_cast<StructType>(T);
  return StructDebugInfo[Ty];
}

StructFieldAccessInfo* StructFieldAccessManager::createOrGetStructFieldAccessInfo(const Type* T, const Function* F)
{
  if (auto* def = getStructFieldAccessInfo(T))
    return def;
  else{
    assert(T->isStructTy() && isa<StructType>(T));
    auto* debugInfo = retrieveDebugInfoForStruct(T);
    def = StructFieldAccessInfoMap[T] = new StructFieldAccessInfo(T, CurrentModule, this, debugInfo);
    return def;
  }
}

StructFieldAccessInfo* StructFieldAccessManager::getStructFieldAccessInfo(const Type* T) const
{
  auto ret = StructFieldAccessInfoMap.find(T);
  if (ret != StructFieldAccessInfoMap.end())
    return ret->second;
  else
    return NULL;
}

Optional<std::pair<const Type*, unsigned> > StructFieldAccessManager::getFieldAccessOnInstruction(const Instruction* I) const
{
  Optional<std::pair<const Type*, unsigned> > ret;
  for (auto &it : StructFieldAccessInfoMap){
    if (auto FieldNum = it.second->getAccessFieldNum(I)){
      return std::pair<const Type*, unsigned>(it.first, FieldNum.getValue());
    }
  }
  return ret;
}

void StructFieldAccessManager::debugPrintAllStructAccesses()
{
  dbgs() << "------------ Printing all struct accesses: ---------------- \n";
  for (auto &it : StructFieldAccessInfoMap){
    dbgs().changeColor(raw_ostream::YELLOW);
    auto* type = it.first;
    assert(isa<StructType>(type));
    if (dyn_cast<StructType>(type)->isLiteral()){
      dbgs() << "A literal struct has accesses: \n";
    }
    else{
      dbgs() << "Struct [" << type->getStructName() << "] has accesses: \n";
    }
    dbgs().changeColor(raw_ostream::GREEN);
    it.second->debugPrintAllStructAccesses(dbgs());
    dbgs().resetColor();
  }
  dbgs() << "----------------------------------------------------------- \n";
}

void StructFieldAccessManager::debugPrintAnnotatedModule()
{
  StructFieldCacheAnalysisAnnotatedWriter Writer(this);
  std::error_code EC;
  raw_fd_ostream FILE_OS("AnnotatedModule.IR.ll", EC, llvm::sys::fs::F_RW);
  FILE_OS.changeColor(raw_ostream::YELLOW);
  FILE_OS << "Annotated module print\n";
  FILE_OS.resetColor();
  CurrentModule.print(FILE_OS, &Writer);
  FILE_OS.resetColor();
}

void StructFieldAccessManager::printStats()
{
  std::error_code EC;
  raw_fd_ostream FILE_OS("/tmp/SFCA-"+CurrentModule.getName().str()+".csv", EC, llvm::sys::fs::F_RW);
  FILE_OS << "Name," << CurrentModule.getName() << "\n";
  outs() << "------------ Printing stats for struct accesses: ---------------- \n";
  outs().changeColor(raw_ostream::YELLOW);
  outs() << "There are " << StructFieldAccessInfoMap.size() << " struct types are accessed in the program\n";
  FILE_OS << "Total," << StructFieldAccessInfoMap.size() << "\n";
  for (auto &it : StructFieldAccessInfoMap){
    auto* type = it.first;
    assert(isa<StructType>(type));
    auto Result = it.second->getTotalNumFieldAccess();
    if (dyn_cast<StructType>(type)->isLiteral()){
      if (Result){
        outs() << "A literal struct has " << Result << " accesses.\n";
        FILE_OS << "Literal," << Result << "\n";
      }
    }
    else{
      if (Result){
        outs() << "Struct [" << type->getStructName() << "] has " << Result << " accesses.\n";
        FILE_OS << type->getStructName() << "," << Result << "\n";
      }
    }
    for (auto i = 0; i < stats::max_stats; i++){
      StatCounts[i] += it.second->getStats(i);
    }
  }
  outs().resetColor();
  outs().changeColor(raw_ostream::GREEN);
  outs() << "Stats:\n";
  for (auto i = 0; i < stats::max_stats; i++){
    if (StatCounts[i]){
      outs() << "Case " << StatNames[i] << " was found " << StatCounts[i] <<  " times\n";
    }
  };
  FILE_OS << "GEP as Arg," << StatCounts[stats::gep_in_arg] << "\n";
  outs().changeColor(raw_ostream::BLUE);
  outs() << "Stats are stored into " << "/tmp/SFCA-"+CurrentModule.getName().str()+".csv" << "\n";
  outs().resetColor();
  outs() << "----------------------------------------------------------------- \n";
  FILE_OS.close();
}

static void performIRAnalysis(Module &M,
                              StructFieldAccessManager* StructManager)
{
  // Find all global structs
  for (auto &G : M.globals()){
    // Only process globals defined in current module (the scope of whole program)
    if (G.isDeclaration())
      continue;
    DEBUG(dbgs().changeColor(raw_ostream::YELLOW));
    // G is always a pointer
    if (G.getValueType()->isStructTy()){
      DEBUG(dbgs() << "Found a global defined as struct: " << G << "\n");
      auto* structInfoPtr = StructManager->createOrGetStructFieldAccessInfo(G.getValueType(), NULL);
      assert(structInfoPtr);
      structInfoPtr->analyzeUsersOfStructValue(&G);
    }
    // Case for struct*
    else if (G.getValueType()->isPointerTy() && G.getValueType()->getPointerElementType()->isStructTy()){
      DEBUG(dbgs() << "Found a global has struct* type: " << G << "\n");
      auto* structInfoPtr = StructManager->createOrGetStructFieldAccessInfo(G.getValueType()->getPointerElementType(), NULL);
      assert(structInfoPtr);
      structInfoPtr->analyzeUsersOfStructPointerValue(&G);
      //StructManager->addStats(StructFieldAccessManager::stats::struct_pointer);
    }
    // Case for struct**
    else if (G.getType()->isPointerTy() && G.getType()->getPointerElementType()->isPointerTy() && G.getType()->getPointerElementType()->getPointerElementType()->isStructTy()){
      DEBUG(dbgs() << "Found a global has struct** type: " << G << " but we ignored this\n");
      StructManager->addStats(StructFieldAccessManager::stats::struct_pointer_pointer);
    }
    DEBUG(dbgs().resetColor());
  }

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
            DEBUG(dbgs() << "Found an alloca of a struct: " << I << "\n");
            auto* structInfoPtr = StructManager->createOrGetStructFieldAccessInfo(type, &F);
            structInfoPtr->analyzeUsersOfStructValue(&I);
          }
          else if (type->isPointerTy() && type->getPointerElementType()->isStructTy()){
            DEBUG(dbgs() << "Found an alloca of a struct*: " << I << "\n");
            auto* structInfoPtr = StructManager->createOrGetStructFieldAccessInfo(type->getPointerElementType(), &F);
            assert(structInfoPtr);
            structInfoPtr->analyzeUsersOfStructPointerValue(&I);
            //StructManager->addStats(StructFieldAccessManager::stats::struct_pointer);
          }
          else if (type->isPointerTy() && type->getPointerElementType()->isPointerTy() && type->getPointerElementType()->getPointerElementType()->isStructTy()){
            DEBUG(dbgs() << "Found an alloca of a struct**: " << I << " but we ignore this\n");
            StructManager->addStats(StructFieldAccessManager::stats::struct_pointer_pointer);
          }
        }
      }
    }
  }
  //Find uses of structs through function calls
  for (auto &F : M){
    if (F.isDeclaration())
      continue;
    for (auto &AG : F.args()){
      if (AG.getType()->isStructTy()){
        DEBUG(dbgs() << "Found an argument of a struct pass by value: " << AG << " and no support for this yet\n");
        StructManager->addStats(StructFieldAccessManager::stats::func_arg_value);
      }
      if (AG.getType()->isPointerTy() && AG.getType()->getPointerElementType()->isStructTy()){
        // Identified AG is an argument with a struct type
        auto* StructPtr = StructManager->getStructFieldAccessInfo(AG.getType()->getPointerElementType());
        if (StructPtr){
          DEBUG(dbgs() << "Found an argument of a struct defined in the module: " << AG << "\n");
          StructPtr->analyzeUsersOfStructValue(&AG);
        }
        else{
          DEBUG(dbgs() << "Found an argument of a struct not defined in the program: " << AG << "\n");
          StructManager->addStats(StructFieldAccessManager::stats::func_arg_not_defined);
        }
      }
    }
  }
  DEBUG(StructManager->debugPrintAllStructAccesses());
  DEBUG(StructManager->debugPrintAnnotatedModule());
  StructManager->printStats();
}

static bool performStructFieldCacheAnalysis(Module &M,
                                            function_ref<BlockFrequencyInfo *(Function &)> LookupBFI)
{
  DEBUG(dbgs() << "Start of struct field cache analysis\n");
  StructFieldAccessManager StructManager(M, LookupBFI);
  // Step 0 - retrieve debug info for all struct FIXME: disable for now because it's not supporting annonymous structs
  //StructManager.retrieveDebugInfoForAllStructs();
  // Step 1 - perform IR analysis to collect info of all structs
  performIRAnalysis(M, &StructManager);

  DEBUG(dbgs() << "End of struct field cache analysis\n");
  return false;
}

StructFieldCacheAnalysis::StructFieldCacheAnalysis() {}

PreservedAnalyses StructFieldCacheAnalysis::run(Module &M, ModuleAnalysisManager &AM) {
  auto &FAM = AM.getResult<FunctionAnalysisManagerModuleProxy>(M).getManager();
  auto LookupBFI = [&FAM](Function& F) {
    return &FAM.getResult<BlockFrequencyAnalysis>(F);
  };
  if (!performStructFieldCacheAnalysis(M, LookupBFI))
    return PreservedAnalyses::all();
  return PreservedAnalyses::none();
}


bool StructFieldCacheAnalysisPass::runOnModule(Module &M){
  auto LookupBFI = [this](Function& F) {
    return &this->getAnalysis<BlockFrequencyInfoWrapperPass>(F).getBFI();
  };
  return performStructFieldCacheAnalysis(M, LookupBFI);
}
