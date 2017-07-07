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
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Operator.h"
#include "llvm/Pass.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/IPO.h"

#include <unordered_map>
#include <unordered_set>
#include <vector>

using namespace llvm;

#define DEBUG_TYPE "struct-analysis"
#define DEBUG_TYPE_IR "struct-analysis-IR"

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
typedef unsigned FieldNumType;

class StructFieldAccessInfo;
/// This class is used to keep track of all StructFieldAccessInfo objects
/// in the program and make sure only one StructFieldAccessInfo object for
/// each type of struct declared in the program.
class StructFieldAccessManager
{
 public:
  /// enum used to represent different type of struct definitions
  enum StructDefinitionType{
    SDT_GlobalStruct,
    SDT_GlobalStructPtr,
    SDT_GlobalStructPtrPtr,
    SDT_LocalStruct,
    SDT_LocalStructPtr,
    SDT_LocalStructPtrPtr
  };

  /// enum used to count the corner cases in the program for future consideration
  enum DebugStats{
    DS_StructPtrPtr,
    DS_FuncArgValue,
    DS_FuncArgNotDefined,
    DS_GepPassedIntoFunc,
    DS_GepPassedIntoBitcast,
    DS_GepUnknownUse,
    DS_UserNotInstructionNorOperator,
    DS_NoAccess,
    DS_PassedIntoOutsideFunction,
    DS_GepUsedOnStructPtr,
    DS_UnknownUsesOnStructPtr,
    DS_MaxNumStats
  };

  StructFieldAccessManager(const Module& M, function_ref<BlockFrequencyInfo *(Function &)> LBFI):
      CurrentModule(M), LookupBFI(LBFI), StatCounts(DebugStats::DS_MaxNumStats) {};

  /// Check if the struct type is created before; if not, create a new StructFieldAccessInfo object for it
  StructFieldAccessInfo* createOrGetStructFieldAccessInfo(const Type* T, const StructDefinitionType ST);

  /// Retrieve the pointer to the previous created StructFieldAccessInfo object for the type
  StructFieldAccessInfo* getStructFieldAccessInfo(const Type* T) const;

  /// Retrieve execution count for a basic block
  Optional<uint64_t> getExecutionCount(const BasicBlock* BB) const{
    Function* Func = const_cast<Function*>(BB->getParent());
    return LookupBFI(*Func)->getBlockProfileCount(BB);
  }

  /// Retrive a pair of information if the instruction is accessing any struct type and field number
  Optional<std::pair<const Type*, unsigned> > getFieldAccessOnInstruction(const Instruction* I) const;

  /// Print all accesses of all struct types defined in the program
  void debugPrintAllStructAccesses();

  /// Print the IR of the module with annotated information about struct access
  void debugPrintAnnotatedModule();

  /// Increment stats for one category
  void addStats(unsigned Category) { StatCounts[Category]++; }
  /// Print a brief stats of struct access
  void printStats();

 private:
  const Module& CurrentModule;

  /// Function reference that is used to retrive execution count for basic block
  function_ref<BlockFrequencyInfo *(Function &)> LookupBFI;

  /// A map storing access info of all structs
  std::unordered_map<const Type*, StructFieldAccessInfo*> StructFieldAccessInfoMap;

  /// \name Data structure to get statistics of each DebugStats entry
  /// %{
  std::vector<unsigned> StatCounts;

  const std::vector<std::string> StatNames = {
    "Variable type is Struct**",
    "Function argument is a value",
    "Function argument is not defined in the program",
    "GEP value passed into function calls",
    "GEP value passed into bitcast",
    "GEP value passed into unexpected opcode",
    "User is not Instruction nor Operator",
    "Struct defined but no accesses",
    "Struct passed into functions defined out of scope",
    "GEP instruction directly used on struct*",
    "Unknown instruction directly used on struct*"
  };
  /// %}

  /// Used to print name of each StructDefinitionType
  const std::vector<std::string> StructDefinitionTypeNames = {
    "global struct",
    "global struct*",
    "global struct**",
    "local struct",
    "local struct*",
    "local struct**"
  };

};

/// This class is used to store all access information for each struct
/// declared in the program. It records all loads and stores to all fields
/// of the struct to provide essential information for cache-aware struct
/// field analysis.
class StructFieldAccessInfo
{
 public:
  StructFieldAccessInfo(const Type* T, const StructFieldAccessManager::StructDefinitionType ST, const Module& MD, const StructFieldAccessManager* M, const DICompositeType* D):
      Eligiblity(true),
      CurrentModule(MD),
      StructureType(NULL),
      StructDefinition(ST),
      DebugInfo(D),
      NumElements(0),
      StructManager(M),
      StatCounts(StructFieldAccessManager::DebugStats::DS_MaxNumStats)
  {
    assert(T && T->isStructTy());
    StructureType = dyn_cast<StructType>(T);
    NumElements = StructureType->getNumElements();
  }

  ~StructFieldAccessInfo() {}

  StructFieldAccessManager::StructDefinitionType getStructDefinition() const { return StructDefinition; }
  bool isEligible() const { return Eligiblity; }

  /// Analyze a value pointing to a struct and collect struct access from it. It can be allocas/function args/globals
  void analyzeUsersOfStructValue(const Value* V);

  /// Analyze a value pointing to a struct* and collect struct access from it. It can be allocas/function args/globals
  void analyzeUsersOfStructPointerValue(const Value* V);

  /// Obtain which field the instruction is accessing and return no val if not accessing any struct field
  Optional<FieldNumType> getAccessFieldNum(const Instruction* I) const;

  /// Obtain total number of instructions that access the struct fields
  //unsigned getTotalNumFieldAccess() const { return LoadStoreFieldAccessMap.size() + CallInstFieldAccessMap.size(); }
  unsigned getTotalNumFieldAccess() const { return LoadStoreFieldAccessMap.size(); }

  /// Obtain execution count for the BasicBlock/Instruction from profiling info, if any
  /// %{
  Optional<uint64_t> getExecutionCount(const BasicBlock* BB) const {
    return StructManager->getExecutionCount(BB);
  }
  Optional<uint64_t> getExecutionCount(const Instruction* I) const {
    return StructManager->getExecutionCount(I->getParent());
  }
  /// %}

  /// Print all instructions that access any struct field
  void debugPrintAllStructAccesses(raw_ostream& OS);

  /// For stats
  /// %{
  void addStats(unsigned Category, unsigned Opcode = 0) {
    StatCounts[Category]++;
    if (Category == StructFieldAccessManager::DebugStats::DS_GepUnknownUse){
      if (UnknownOpcodes.find(Opcode) == UnknownOpcodes.end())
        UnknownOpcodes[Opcode] = 0;
      else
        UnknownOpcodes[Opcode]++;
    }
  }
  unsigned getStats(unsigned Category) const { return StatCounts[Category]; }
  /// %}

  void printUnknownOpcodes(raw_ostream& OS) const {
    if (UnknownOpcodes.size() == 0)
      return;
    OS << "Unknown opcodes stats: \n";
    for (auto& it : UnknownOpcodes){
      OS << "Opcode " << it.first << ": " << it.second << " times\n";
    }
  }

 private:
  bool Eligiblity;
  const Module& CurrentModule;
  const StructType* StructureType;
  const StructFieldAccessManager::StructDefinitionType StructDefinition;
  const DICompositeType* DebugInfo;
  unsigned NumElements;
  const StructFieldAccessManager* StructManager;

  /// A map records all load/store instructions accessing which field of the structure
  std::unordered_map<const Instruction*, unsigned> LoadStoreFieldAccessMap;
  std::vector<unsigned> StatCounts;
  std::unordered_map<unsigned, unsigned> UnknownOpcodes;

 private:
  /// Calculate which field of the struct is the GEP pointing to, from GetElementPtrInst or GEPOperator
  FieldNumType calculateFieldNumFromGEP(const User* U) const;

  /// Record all users of a GEP instruction/operator that calculates the address of a field.
  void addFieldAccessFromGEP(const User* U);

  /// Record an access pattern in the data structure for a load/store
  void addFieldAccessNum(const Instruction* I, FieldNumType FieldNum);
};

/// This class is inherited from AssemblyAnnotationWriter and used
/// to print annotated information on IR
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
        OS << "; [Field " << pair.getValue().second << " of a literal struct.] ";
      else
        OS << "; [Field " << pair.getValue().second << " of struct " << type->getStructName() << "] ";
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
void StructFieldAccessInfo::addFieldAccessNum(const Instruction* I, FieldNumType FieldNum)
{
  assert(I->getOpcode() == Instruction::Load || I->getOpcode() == Instruction::Store); // Only loads and stores
  assert (LoadStoreFieldAccessMap.find(I) == LoadStoreFieldAccessMap.end());
  LoadStoreFieldAccessMap[I] = FieldNum;
}

Optional<FieldNumType> StructFieldAccessInfo::getAccessFieldNum(const Instruction* I) const
{
  Optional<FieldNumType> ret;
  auto it = LoadStoreFieldAccessMap.find(I);
  if (it != LoadStoreFieldAccessMap.end()){
    ret = it->second;
  }
  return ret;
}

FieldNumType StructFieldAccessInfo::calculateFieldNumFromGEP(const User* U) const
{
  DEBUG_WITH_TYPE(DEBUG_TYPE_IR, dbgs() << "Calculating field number from GEP: " << *U << "\n");
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
  auto Offset = (FieldNumType)Index->getUniqueInteger().getZExtValue();
  assert(Offset < NumElements);
  //TODO: ignore indices after this one. If there's indices, the field has to be an array or struct
  return Offset+1; // return field number starting from 1
}

void StructFieldAccessInfo::addFieldAccessFromGEP(const User* U)
{
  DEBUG_WITH_TYPE(DEBUG_TYPE_IR, dbgs() << "Analyze all users of GEP: " << *U << "\n");
  assert(isa<GetElementPtrInst>(U) || isa<GEPOperator>(U));
  auto FieldLoc = calculateFieldNumFromGEP(U);
  if (FieldLoc == 0)
    return;
  for (auto *User : U->users()){
    DEBUG_WITH_TYPE(DEBUG_TYPE_IR, dbgs() << "Check user of " << *U << ": " << *User << "\n");
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
          addStats(StructFieldAccessManager::DebugStats::DS_GepPassedIntoFunc);
        }
        else if (Inst->getOpcode() == Instruction::BitCast){
          addStats(StructFieldAccessManager::DebugStats::DS_GepPassedIntoBitcast);
        }
        else{
          // TODO: Collect stats of this kind of access and add analysis later
          addStats(StructFieldAccessManager::DebugStats::DS_GepUnknownUse, Inst->getOpcode());
        }
      }
    }
    else if (isa<Operator>(User)){
      auto* Inst = dyn_cast<Operator>(U);
      assert (Inst->getOpcode() != Instruction::Load || Inst->getOpcode() != Instruction::Store
              || Inst->getOpcode() != Instruction::Call || Inst->getOpcode() != Instruction::Invoke);
      if (Inst->getOpcode() == Instruction::BitCast){
        addStats(StructFieldAccessManager::DebugStats::DS_GepPassedIntoBitcast);
      }
      else{
        // TODO: Collect stats of this kind of access and add analysis later
        addStats(StructFieldAccessManager::DebugStats::DS_GepUnknownUse, Inst->getOpcode());
      }
    }
    else {
      addStats(StructFieldAccessManager::DebugStats::DS_UserNotInstructionNorOperator);
    }
  }
}

void StructFieldAccessInfo::analyzeUsersOfStructValue(const Value* V)
{
  assert(V->getType()->isPointerTy() && V->getType()->getPointerElementType()->isStructTy());
  for (auto *U : V->users()){
    DEBUG_WITH_TYPE(DEBUG_TYPE_IR, dbgs() << "Analyzing user of " << *V << ": " << *U << "\n");
    //assert(isa<Instruction>(U) || isa<Operator>(U));
    if (isa<Instruction>(U)){
      auto *Inst = dyn_cast<Instruction>(U);
      if (Inst->getOpcode() != Instruction::GetElementPtr){
        // Only support access struct through GEP for now
        if (Inst->getOpcode() == Instruction::Call){
          auto* F = dyn_cast<CallInst>(Inst)->getCalledFunction();
          if (F->isDeclaration()){
            // If a struct is passed to a function not declared in the program, we can't analyze it...
            Eligiblity = false;
          }
        }
        else if (Inst->getOpcode() == Instruction::Invoke){
          auto* F = dyn_cast<InvokeInst>(Inst)->getCalledFunction();
          if (F->isDeclaration()){
            // If a struct is passed to a function not declared in the program, we can't analyze it...
            Eligiblity = false;
          }
        }
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
      addStats(StructFieldAccessManager::DebugStats::DS_UserNotInstructionNorOperator);
    }
  }
}

void StructFieldAccessInfo::analyzeUsersOfStructPointerValue(const Value* V)
{
  // Analyze users of value defined as struct*
  assert(V->getType()->isPointerTy() && V->getType()->getPointerElementType()->isPointerTy() && V->getType()->getPointerElementType()->getPointerElementType()->isStructTy());
  for (auto *U : V->users()){
    DEBUG_WITH_TYPE(DEBUG_TYPE_IR, dbgs() << "Analyzing user of " << *V << ": " << *U << "\n");
    if (isa<Instruction>(U)){
      auto *Inst = dyn_cast<Instruction>(U);
      if (Inst->getOpcode() == Instruction::Load){
        analyzeUsersOfStructValue(Inst);
      }
      else if (Inst->getOpcode() == Instruction::GetElementPtr){
        addStats(StructFieldAccessManager::DebugStats::DS_GepUsedOnStructPtr);
      }
      else{
        addStats(StructFieldAccessManager::DebugStats::DS_UnknownUsesOnStructPtr);
      }
    }
    else if (isa<Operator>(U)){
      auto *Inst = dyn_cast<Operator>(U);
      if (Inst->getOpcode() == Instruction::Load){
        analyzeUsersOfStructValue(Inst);
      }
      else if (Inst->getOpcode() == Instruction::GetElementPtr){
        addStats(StructFieldAccessManager::DebugStats::DS_GepUsedOnStructPtr);
      }
      else{
        addStats(StructFieldAccessManager::DebugStats::DS_UnknownUsesOnStructPtr);
      }
    }
    else{
      addStats(StructFieldAccessManager::DebugStats::DS_UserNotInstructionNorOperator);
    }
  }
}

void StructFieldAccessInfo::debugPrintAllStructAccesses(raw_ostream& OS)
{
  for (auto &it : LoadStoreFieldAccessMap){
    OS << "\tInstruction [" << *it.first << "] accesses field number [" << it.second << "]\n";
  }
}

// Functions for StructFieldAccessManager
StructFieldAccessInfo* StructFieldAccessManager::createOrGetStructFieldAccessInfo(const Type* T, const StructDefinitionType SType)
{
  if (auto* def = getStructFieldAccessInfo(T))
    return def;
  else{
    assert(T->isStructTy() && isa<StructType>(T));
    //FIXME: retrieve debug info of the struct first: auto* debugInfo = retrieveDebugInfoForStruct(T);
    def = StructFieldAccessInfoMap[T] = new StructFieldAccessInfo(T, SType, CurrentModule, this, NULL);
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

Optional<std::pair<const Type*, FieldNumType> > StructFieldAccessManager::getFieldAccessOnInstruction(const Instruction* I) const
{
  Optional<std::pair<const Type*, FieldNumType> > ret;
  for (auto &it : StructFieldAccessInfoMap){
    if (auto FieldNum = it.second->getAccessFieldNum(I)){
      return std::make_pair(it.first, FieldNum.getValue());
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
    assert(Result);
    if (dyn_cast<StructType>(type)->isLiteral()){
      outs() << "A literal struct defined as " << StructDefinitionTypeNames[it.second->getStructDefinition()] << " has " << Result << " accesses.\n";
      FILE_OS << "Literal," << Result << "\n";
    }
    else{
      outs() << "Struct [" << type->getStructName() << "] defined as " << StructDefinitionTypeNames[it.second->getStructDefinition()] << " has " << Result << " accesses.\n";
      FILE_OS << type->getStructName() << "," << Result << "\n";
    }
  }
  outs().resetColor();
  outs().changeColor(raw_ostream::GREEN);
  outs() << "Stats:\n";
  for (auto &it : StructFieldAccessInfoMap){
    for (auto i = 0; i < DebugStats::DS_MaxNumStats; i++){
      StatCounts[i] += it.second->getStats(i);
      if (i == DebugStats::DS_GepUnknownUse){
        it.second->printUnknownOpcodes(outs());
      }
    }
  }
  for (auto i = 0; i < DebugStats::DS_MaxNumStats; i++){
    if (StatCounts[i]){
      outs() << "Case " << StatNames[i] << " was found " << StatCounts[i] <<  " times\n";
    }
  };
  FILE_OS << "GEP as Arg," << StatCounts[DebugStats::DS_GepPassedIntoFunc] << "\n";
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
    DEBUG_WITH_TYPE(DEBUG_TYPE_IR, dbgs().changeColor(raw_ostream::YELLOW));
    // G is always a pointer
    if (G.getValueType()->isStructTy()){
      DEBUG_WITH_TYPE(DEBUG_TYPE_IR, dbgs() << "Found a global defined as struct: " << G << "\n");
      auto* structInfoPtr = StructManager->createOrGetStructFieldAccessInfo(G.getValueType(), StructFieldAccessManager::StructDefinitionType::SDT_GlobalStruct);
      assert(structInfoPtr);
      structInfoPtr->analyzeUsersOfStructValue(&G);
    }
    // Case for struct*
    else if (G.getValueType()->isPointerTy() && G.getValueType()->getPointerElementType()->isStructTy()){
      DEBUG_WITH_TYPE(DEBUG_TYPE_IR, dbgs() << "Found a global has struct* type: " << G << "\n");
      auto* structInfoPtr = StructManager->createOrGetStructFieldAccessInfo(G.getValueType()->getPointerElementType(), StructFieldAccessManager::StructDefinitionType::SDT_GlobalStructPtr);
      assert(structInfoPtr);
      structInfoPtr->analyzeUsersOfStructPointerValue(&G);
    }
    // Case for struct**
    else if (G.getType()->isPointerTy() && G.getType()->getPointerElementType()->isPointerTy() && G.getType()->getPointerElementType()->getPointerElementType()->isStructTy()){
      DEBUG_WITH_TYPE(DEBUG_TYPE_IR, dbgs() << "Found a global has struct** type: " << G << " but we ignored this\n");
      StructManager->addStats(StructFieldAccessManager::DebugStats::DS_StructPtrPtr);
    }
    DEBUG_WITH_TYPE(DEBUG_TYPE_IR, dbgs().resetColor());
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
            DEBUG_WITH_TYPE(DEBUG_TYPE_IR, dbgs() << "Found an alloca of a struct: " << I << "\n");
            auto* structInfoPtr = StructManager->createOrGetStructFieldAccessInfo(type, StructFieldAccessManager::StructDefinitionType::SDT_LocalStruct);
            structInfoPtr->analyzeUsersOfStructValue(&I);
          }
          else if (type->isPointerTy() && type->getPointerElementType()->isStructTy()){
            DEBUG_WITH_TYPE(DEBUG_TYPE_IR, dbgs() << "Found an alloca of a struct*: " << I << "\n");
            auto* structInfoPtr = StructManager->createOrGetStructFieldAccessInfo(type->getPointerElementType(), StructFieldAccessManager::StructDefinitionType::SDT_LocalStructPtr);
            assert(structInfoPtr);
            structInfoPtr->analyzeUsersOfStructPointerValue(&I);
          }
          else if (type->isPointerTy() && type->getPointerElementType()->isPointerTy() && type->getPointerElementType()->getPointerElementType()->isStructTy()){
            DEBUG_WITH_TYPE(DEBUG_TYPE_IR, dbgs() << "Found an alloca of a struct**: " << I << " but we ignore this\n");
            StructManager->addStats(StructFieldAccessManager::DebugStats::DS_StructPtrPtr);
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
        DEBUG_WITH_TYPE(DEBUG_TYPE_IR, dbgs() << "Found an argument of a struct pass by value: " << AG << " and no support for this yet\n");
        StructManager->addStats(StructFieldAccessManager::DebugStats::DS_FuncArgValue);
      }
      if (AG.getType()->isPointerTy() && AG.getType()->getPointerElementType()->isStructTy()){
        // Identified AG is an argument with a struct type
        auto* StructPtr = StructManager->getStructFieldAccessInfo(AG.getType()->getPointerElementType());
        if (StructPtr){
          DEBUG_WITH_TYPE(DEBUG_TYPE_IR, dbgs() << "Found an argument of a struct defined in the module: " << AG << "\n");
          StructPtr->analyzeUsersOfStructValue(&AG);
        }
        else{
          DEBUG_WITH_TYPE(DEBUG_TYPE_IR, dbgs() << "Found an argument of a struct not defined in the program: " << AG << "\n");
          StructManager->addStats(StructFieldAccessManager::DebugStats::DS_FuncArgNotDefined);
        }
      }
    }
  }
  // Summarizes all uses of fields in function calls
  DEBUG(StructManager->debugPrintAllStructAccesses());
  DEBUG(StructManager->debugPrintAnnotatedModule());
}

static bool performStructFieldCacheAnalysis(Module &M,
                                            function_ref<BlockFrequencyInfo *(Function &)> LookupBFI)
{
  DEBUG(dbgs() << "Start of struct field cache analysis\n");
  StructFieldAccessManager StructManager(M, LookupBFI);
  // Step 0 - retrieve debug info for all struct TODO: disable for now because it's not supporting annonymous structs
  //StructManager.retrieveDebugInfoForAllStructs();
  // Step 1 - perform IR analysis to collect info of all structs
  performIRAnalysis(M, &StructManager);
  StructManager.printStats();
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
