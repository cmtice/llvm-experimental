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
#include "llvm/Analysis/BranchProbabilityInfo.h"
#include "llvm/IR/AssemblyAnnotationWriter.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Operator.h"
#include "llvm/Pass.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/IPO.h"

#include <queue>
#include <unordered_set>
#include <unordered_map>
#include <unordered_set>
#include <vector>

using namespace llvm;

#define DEBUG_TYPE "struct-analysis"

// Macros used to turn on detailed debug info for each step
#define DEBUG_TYPE_IR "struct-analysis-IR"
#define DEBUG_TYPE_FRG "struct-analysis-FRG"

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
INITIALIZE_PASS_DEPENDENCY(BranchProbabilityInfoWrapperPass)
INITIALIZE_PASS_END(StructFieldCacheAnalysisPass, "struct-field-cache-analysis", "Struct Field Cache Analysis", false, false)
ModulePass *llvm::createStructFieldCacheAnalysisPass() { return new StructFieldCacheAnalysisPass; }

namespace llvm{
typedef unsigned ExecutionCountType;
typedef unsigned DataBytesType;
typedef unsigned FieldNumType;

class FieldReferenceGraph
{
 public:
  struct Edge;
  struct Node{
    Node(unsigned I, FieldNumType N): Id(I), FieldNum(N), InSum(0), OutSum(0) {}
    unsigned Id;
    FieldNumType FieldNum;
    std::unordered_set<Edge*> InEdges;
    std::unordered_set<Edge*> OutEdges;
    ExecutionCountType InSum;
    ExecutionCountType OutSum;
  };

  struct Edge{
   public:
    Edge(unsigned I, ExecutionCountType C, DataBytesType D): Id(I), ExecutionCount(C), DataSize(D){}
    void connectNodes(Node* From, Node* To) { FromNode = From; ToNode = To; }
    unsigned Id;
    ExecutionCountType ExecutionCount;
    DataBytesType DataSize;
    Node* FromNode;
    Node* ToNode;
  };

  struct BasicBlockHelperInfo{
    BasicBlockHelperInfo(): RemainBytes(0), FirstNode(NULL), LastNode(NULL) {}
    DataBytesType RemainBytes;
    Node* FirstNode;
    Node* LastNode;
  };

 public:
  FieldReferenceGraph(const Function* F) : Func(F), EntryNode(NULL) {}
  ~FieldReferenceGraph() {
    for (auto *N : NodeList)
      delete N;
    for (auto *E : EdgeList)
      delete E;
    for (auto &it : BBInfoMap)
      delete it.second;
    NodeList.clear();
    EdgeList.clear();
    BBInfoMap.clear();
  }

  // Creator and getter of helper info for the basic block, useful when connect nodes from different basic blocks
  BasicBlockHelperInfo* createBasicBlockHelperInfo(const BasicBlock* BB);
  BasicBlockHelperInfo* getBasicBlockHelperInfo(const BasicBlock* BB);

  // The two functions are used to create a new node in the graph, unconnected with other nodes, and return the pointer to the Node
  Node* createNewNode(FieldNumType FieldNum);
  Node* createNewNode() { return createNewNode(0); }

  // The two functions are used to connect two nodes in FRG with or without given weight
  void connectNodes(Node* From, Node* To, ExecutionCountType C, DataBytesType D);
  void connectNodes(Node* From, Node* To) { connectNodes(From, To, 0, 0); }

  // The getter and setter of entry node in the FRG
  Node* getEntryNode() const { return EntryNode; }
  void setEntryNode(Node* N) { EntryNode = N; }

  // For debug
  void debugPrint(raw_ostream& OS) const;

 private:
  const Function* Func;
  Node* EntryNode;
  std::vector<Node*> NodeList;
  std::vector<Edge*> EdgeList;
  std::unordered_map<const BasicBlock*, BasicBlockHelperInfo*> BBInfoMap;
};

class StructFieldAccessInfo;
class StructFieldAccessManager
{
  /* This class is used to keep track of all StructFieldAccessInfo objects in the program
     and make sure only one StructFieldAccessInfo object for each type of struct declared
     in the program.
  */
 public:
  StructFieldAccessManager(const Module& M,
                           function_ref<BlockFrequencyInfo *(Function &)> LBFI,
                           function_ref<BranchProbabilityInfo *(Function &)> LBPI):
      CurrentModule(M), LookupBFI(LBFI), LookupBPI(LBPI), StatCounts(Stats::max_stats) {};
  ~StructFieldAccessManager();

  // Retrieve debug info for all structs defined in the program
  void retrieveDebugInfoForAllStructs();
  // Functions for IR analysis
  // Check if the struct type is created before; if not, create a new StructFieldAccessInfo object for it
  StructFieldAccessInfo* createOrGetStructFieldAccessInfo(const Type* T);
  // Retrieve the pointer to the previous created StructFieldAccessInfo object for the type
  StructFieldAccessInfo* getStructFieldAccessInfo(const Type* T) const;
  // Retrieve execution count for a basic block
  Optional<uint64_t> getExecutionCount(const BasicBlock* BB) const{
    Function* func = const_cast<Function*>(BB->getParent());
    return LookupBFI(*func)->getBlockProfileCount(BB);
  }
  Optional<double> getBranchProbability(const BasicBlock* FromBB, const BasicBlock* ToBB) const{
    assert(FromBB->getParent() == ToBB->getParent());
    Function* func = const_cast<Function*>(FromBB->getParent());
    auto Prob = LookupBPI(*func)->getEdgeProbability(FromBB, ToBB);
    return 1.0 * Prob.getNumerator() / Prob.getDenominator();
  }
  // Retrive a pair of information if the instruction is accessing any struct type and field number
  Optional<std::pair<const Type*, FieldNumType> > getFieldAccessOnInstruction(const Instruction* I) const;

  // Functions for FRG build
  // Call functions to build FRG for all structs with accesses
  void buildFieldReferenceGraphForAllStructs();

  // Functions for debugging
  // Print all accesses of all struct types defined in the program
  void debugPrintAllStructAccesses() const;
  // Print all FRGs of all struct types
  void debugPrintAllFRGs() const;
  // Print the IR of the module with annotated information about struct access
  void debugPrintAnnotatedModule() const;

  // For stats
  enum Stats{
    struct_pointer_pointer,
    func_arg_value,
    func_arg_not_defined,
    gep_in_arg,
    gep_in_bitcast,
    gep_in_unknown,
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
  function_ref<BranchProbabilityInfo *(Function &)> LookupBPI;
  std::unordered_map<const Type*, StructFieldAccessInfo*> StructFieldAccessInfoMap;
  std::vector<unsigned> StatCounts;
  const std::vector<std::string> StatNames = {"Variable type is Struct**",
                                              "Function argument is a value",
                                              "Function argument is not defined in the program",
                                              "GEP value passed into function calls",
                                              "GEP value passed into bitcast",
                                              "GEP value passed into unexpected opcode",
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
  StructFieldAccessInfo(const Type* T, const Module& MD, const StructFieldAccessManager* M, const DICompositeType* D): CurrentModule(MD), StructureType(dyn_cast<StructType>(T)), DebugInfo(D), NumElements(StructureType->getNumElements()), StructManager(M), StatCounts(StructFieldAccessManager::Stats::max_stats) {}
  ~StructFieldAccessInfo() {
    for (auto* it : FRGArray){
      delete it;
    }
    FRGArray.clear();
  }

  // Functions for IR analysis
  // Analyze a value pointing to a struct and collect struct access from it. It can be allocas/function args/globals
  void analyzeUsersOfStructValue(const Value* V);
  // Analyze a value pointing to a struct* and collect struct access from it. It can be allocas/function args/globals
  void analyzeUsersOfStructPointerValue(const Value* V);
  // Obtain which field the instruction is accessing and return no val if not accessing any struct field
  Optional<FieldNumType> getAccessFieldNum(const Instruction* I) const;
  // Obtain total number of instructions that access the struct fields
  unsigned getTotalNumFieldAccess() const { return FieldAccessMap.size(); }
  // Obtain execution count for the BasicBlock/Instruction from profiling info, if any
  Optional<uint64_t> getExecutionCount(const BasicBlock* BB) const {
    return StructManager->getExecutionCount(BB);
  }
  Optional<uint64_t> getExecutionCount(const Instruction* I) const {
    return StructManager->getExecutionCount(I->getParent());
  }
  Optional<double> getBranchProbability(const BasicBlock* FromBB, const BasicBlock* ToBB) const{
    return StructManager->getBranchProbability(FromBB, ToBB);
  }
  // Functions for building FRG
  void buildFieldReferenceGraph();
  void buildFieldReferenceGraph(const Function* FOA);

  // Print all instructions that access any struct field
  void debugPrintAllStructAccesses(raw_ostream& OS) const;
  // Print all FRGs for this struct
  void debugPrintFieldReferenceGraph(raw_ostream& OS) const;

  // For stats
  void addStats(unsigned Category, unsigned Opcode = 0) {
    StatCounts[Category]++;
    if (Category == StructFieldAccessManager::Stats::gep_in_unknown){
      UnknownOpcodes.insert(Opcode);
    }
  }
  unsigned getStats(unsigned Category) const { return StatCounts[Category]; }
  void printUnknownOpcodes(raw_ostream& OS) const {
    if (UnknownOpcodes.size() == 0)
      return;
    OS << "Unknown opcodes: ";
    for (auto& it : UnknownOpcodes){
      OS << it << " ";
    }
    OS << "\n";
  }

 private:
  const Module& CurrentModule;
  const StructType* StructureType;
  const DICompositeType* DebugInfo;
  FieldNumType NumElements;
  const StructFieldAccessManager* StructManager;
  // A map records all instructions accessing which field of the structure
  std::unordered_map<const Instruction*, FieldNumType> FieldAccessMap;
  // A vector stores functions that contains struct accesses for further analysis
  std::unordered_set<const Function*> FunctionsForAnalysis;
  // A vector stores all FRGs for all functions
  std::vector<FieldReferenceGraph*> FRGArray;
  //A vector stores per struct stats
  std::vector<unsigned> StatCounts;
  std::unordered_set<unsigned> UnknownOpcodes;

  // Private functions
  // Calculate which field of the struct is the GEP pointing to, from GetElementPtrInst or GEPOperator
  FieldNumType calculateFieldNumFromGEP(const User* U) const;
  // Record all users of a GEP instruction/operator that calculates the address of a field.
  // It's the only supported way to add access to a field for now
  void addFieldAccessFromGEP(const User* U);
  // Record an access pattern in the data structure
  void addFieldAccessNum(const Instruction* I, FieldNumType FieldNum);

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

// Functions for FieldReferenceGraph
FieldReferenceGraph::BasicBlockHelperInfo* FieldReferenceGraph::createBasicBlockHelperInfo(const BasicBlock* BB)
{
  assert (BBInfoMap.find(BB) == BBInfoMap.end());
  auto* ret = BBInfoMap[BB] = new BasicBlockHelperInfo;
  return ret;
}
FieldReferenceGraph::BasicBlockHelperInfo* FieldReferenceGraph::getBasicBlockHelperInfo(const BasicBlock* BB)
{
  assert (BBInfoMap.find(BB) != BBInfoMap.end());
  return BBInfoMap[BB];
}
FieldReferenceGraph::Node* FieldReferenceGraph::createNewNode(FieldNumType FieldNum)
{
  auto* Node = new FieldReferenceGraph::Node(NodeList.size(), FieldNum);
  NodeList.push_back(Node);
  return Node;
}
void FieldReferenceGraph::connectNodes(FieldReferenceGraph::Node* From, FieldReferenceGraph::Node* To, ExecutionCountType C, DataBytesType D)
{
  auto* Edge = new FieldReferenceGraph::Edge(EdgeList.size(), C, D);
  EdgeList.push_back(Edge);
  Edge->connectNodes(From, To);
  From->OutEdges.insert(Edge);
  To->InEdges.insert(Edge);
  From->OutSum += C;
  To->InSum += C;
}

void FieldReferenceGraph::debugPrint(raw_ostream& OS) const
{
  assert(EntryNode);
  std::queue<Node*> ExamineList; // List of Nodes to print
  std::unordered_set<Node*> ExaminedSet; // Set of Nodes popped from list to avoid duplicate
  ExamineList.push(EntryNode);
  OS << "Field Reference Graph for function: " << Func->getName() << "\n";
  while (!ExamineList.empty()){
    auto* Node = ExamineList.front();
    ExamineList.pop();
    if (ExaminedSet.find(Node) != ExaminedSet.end())
      continue;
    ExaminedSet.insert(Node);
    OS << "Node " << Node->Id << " accesses " << Node->FieldNum << ": ";
    OS << "connect with {";
    for (auto* Edge : Node->OutEdges){
      OS << " Node " << Edge->ToNode->Id << " (" << Edge->ExecutionCount << "," << Edge->DataSize << ")  ";
      ExamineList.push(Edge->ToNode);
    }
    OS << "}\n";
  }
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

void StructFieldAccessInfo::addFieldAccessNum(const Instruction* I, FieldNumType FieldNum)
{
  assert(I->getOpcode() == Instruction::Load || I->getOpcode() == Instruction::Store); // Only loads and stores
  assert (FieldAccessMap.find(I) == FieldAccessMap.end());
  FieldAccessMap[I] = FieldNum;
  FunctionsForAnalysis.insert(I->getParent()->getParent()); // If this function already in the set, it'll be ignored by the set::insert
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

Optional<FieldNumType> StructFieldAccessInfo::getAccessFieldNum(const Instruction* I) const
{
  Optional<FieldNumType> ret;
  auto it = FieldAccessMap.find(I);
  if (it != FieldAccessMap.end()){
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
          addStats(StructFieldAccessManager::Stats::gep_in_arg);
        }
        else if (Inst->getOpcode() == Instruction::BitCast){
          addStats(StructFieldAccessManager::Stats::gep_in_bitcast);
        }
        else{
          addStats(StructFieldAccessManager::Stats::gep_in_unknown, Inst->getOpcode());
        }
      }
    }
    else if (isa<Operator>(User)){
      auto* Inst = dyn_cast<Operator>(U);
      assert (Inst->getOpcode() != Instruction::Load || Inst->getOpcode() != Instruction::Store
              || Inst->getOpcode() != Instruction::Call || Inst->getOpcode() != Instruction::Invoke);
      if (Inst->getOpcode() == Instruction::BitCast){
        addStats(StructFieldAccessManager::Stats::gep_in_bitcast);
      }
      else{
        addStats(StructFieldAccessManager::Stats::gep_in_unknown, Inst->getOpcode());
      }
    }
    else{
      addStats(StructFieldAccessManager::Stats::user_not_instruction_nor_operator);
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
      addStats(StructFieldAccessManager::Stats::user_not_instruction_nor_operator);
    }
  }
}

void StructFieldAccessInfo::analyzeUsersOfStructPointerValue(const Value* V)
{
  // Analyze users of value defined as struct*
  assert(V->getType()->isPointerTy() && V->getType()->getPointerElementType()->isPointerTy() && V->getType()->getPointerElementType()->getPointerElementType()->isStructTy());
  for (auto *U : V->users()){
    DEBUG_WITH_TYPE(DEBUG_TYPE_IR, dbgs() << "Analyzing user of " << *V << ": " << *U << "\n");
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
      addStats(StructFieldAccessManager::Stats::user_not_instruction_nor_operator);
    }
  }
}

void StructFieldAccessInfo::buildFieldReferenceGraph(const Function* F)
{
  DEBUG_WITH_TYPE(DEBUG_TYPE_FRG, dbgs() << "Create a new empty FRG\n");
  auto* FRG = new FieldReferenceGraph(F);
  FRGArray.push_back(FRG);
  // Create and connect node inside each basic block
  for (auto &BB : *F){
    DEBUG_WITH_TYPE(DEBUG_TYPE_FRG, dbgs() << "Build partial FRG for BB: " << BB << "\n");
    auto* BBI = FRG->createBasicBlockHelperInfo(&BB);
    for (auto &I : BB){
      if (auto FieldNum = getAccessFieldNum(&I)){
        // Case that I is a struct access
        DEBUG_WITH_TYPE(DEBUG_TYPE_FRG, dbgs() << "Found an instruction " << I << " is a struct access on field [" << FieldNum.getValue() << "]\n");
        auto* NewNode = FRG->createNewNode(FieldNum.getValue());
        if (BBI->LastNode){
          DEBUG_WITH_TYPE(DEBUG_TYPE_FRG, dbgs() << "Previous nodes found in the BB\n");
          auto C = 0;
          if (auto ExCnt = getExecutionCount(&I))
            C = getExecutionCount(&I).getValue();
          auto D = BBI->RemainBytes;
          FRG->connectNodes(BBI->LastNode, NewNode, C, D);
          BBI->RemainBytes = 0;
          DEBUG_WITH_TYPE(DEBUG_TYPE_FRG, dbgs() << "Connect new node with previous node in the BB: Field [" << BBI->LastNode->FieldNum << "] to [" << FieldNum.getValue() << "] with count: " << C << " and data " << D << " bytes \n");
          BBI->LastNode = NewNode;
        }
        else{
          DEBUG_WITH_TYPE(DEBUG_TYPE_FRG, dbgs() << "No previous node found. It is the first node\n");
          BBI->FirstNode = BBI->LastNode = NewNode;
        }
      }
      else{
        // Case that I is not struct access but a memory access
        if (I.getOpcode() == Instruction::Load || I.getOpcode() == Instruction::Store){
          DEBUG_WITH_TYPE(DEBUG_TYPE_FRG, dbgs() << "Found an instruction " << I << " is not a struct access but a load/store.\n");
          if (BBI->LastNode == NULL){
            // Create a dummy node for the first non-struct memory access
            DEBUG_WITH_TYPE(DEBUG_TYPE_FRG, dbgs() << "Create a dummy node as the first node in the BB.\n");
            BBI->FirstNode = BBI->LastNode = FRG->createNewNode();
          }
          else{
            Type* type;
            if (I.getOpcode() == Instruction::Load)
              type = I.getType();
            else
              type = I.getOperand(0)->getType();
            assert (type->isSized());
            BBI->RemainBytes += CurrentModule.getDataLayout().getTypeSizeInBits(type) / 8;
            DEBUG_WITH_TYPE(DEBUG_TYPE_FRG, dbgs() << "Increment remaining data size to " << BBI->RemainBytes << "\n");
          }
        }
      }
    }
    if (BBI->LastNode == NULL){
      assert(BBI->FirstNode == NULL);
      DEBUG_WITH_TYPE(DEBUG_TYPE_FRG, dbgs() << "Create a dummy node as BB does not have memory accesses\n");
      BBI->FirstNode = BBI->LastNode = FRG->createNewNode();
    }
  }
  // Connect nodes between different basic blocks
  for (auto &BB : *F){
    auto* BBI = FRG->getBasicBlockHelperInfo(&BB);
    auto* Term = BB.getTerminator();
    for (const auto *SB : Term->successors()){
      auto* SBI = FRG->getBasicBlockHelperInfo(SB);
      ExecutionCountType C = 0;
      auto BBCount = getExecutionCount(&BB); //, SBCount = getExecutionCount(SB);
      if (BBCount.hasValue()){
        //C = std::min(BBCount.getValue(), SBCount.getValue());
        // Take probability of the branch
        auto Prob = getBranchProbability(&BB, SB);
        assert(Prob.hasValue());
        C = BBCount.getValue() * Prob.getValue();
      }
      auto D = BBI->RemainBytes; // Use size of remaining data in BB
      FRG->connectNodes(BBI->LastNode, SBI->FirstNode, C, D);
    }
  }
  auto* BBI = FRG->getBasicBlockHelperInfo(&F->getEntryBlock());
  FRG->setEntryNode(BBI->FirstNode);
}

void StructFieldAccessInfo::buildFieldReferenceGraph()
{
  for (auto *F : FunctionsForAnalysis){
    buildFieldReferenceGraph(F);
  }
}

void StructFieldAccessInfo::debugPrintFieldReferenceGraph(raw_ostream& OS) const
{
  for (auto *FRG : FRGArray){
    FRG->debugPrint(OS);
  }
}

void StructFieldAccessInfo::debugPrintAllStructAccesses(raw_ostream& OS) const
{
  for (auto &it : FieldAccessMap){
    OS << "\tInstruction [" << *it.first << "] accesses field number [" << it.second << "]\n";
  }
}

// Functions for StructFieldAccessManager
StructFieldAccessManager::~StructFieldAccessManager() {
  for (auto &it : StructFieldAccessInfoMap)
    delete it.second;
  StructFieldAccessInfoMap.clear();
  DEBUG(dbgs() << "Finish StructFieldAccessManager destructor\n");
}

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

StructFieldAccessInfo* StructFieldAccessManager::createOrGetStructFieldAccessInfo(const Type* T)
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

Optional<std::pair<const Type*, FieldNumType> > StructFieldAccessManager::getFieldAccessOnInstruction(const Instruction* I) const
{
  Optional<std::pair<const Type*, FieldNumType> > ret;
  for (auto &it : StructFieldAccessInfoMap){
    if (auto FieldNum = it.second->getAccessFieldNum(I)){
      return std::pair<const Type*, FieldNumType>(it.first, FieldNum.getValue());
    }
  }
  return ret;
}

void StructFieldAccessManager::buildFieldReferenceGraphForAllStructs()
{
  for (auto& it : StructFieldAccessInfoMap){
    if (it.second->getTotalNumFieldAccess() != 0){
      it.second->buildFieldReferenceGraph();
    }
  }
}

void StructFieldAccessManager::debugPrintAllStructAccesses() const
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

void StructFieldAccessManager::debugPrintAllFRGs() const
{
  dbgs() << "------------ Printing all FRGs: ------------------- \n";
  for (auto &it : StructFieldAccessInfoMap){
    dbgs().changeColor(raw_ostream::YELLOW);
    auto* type = it.first;
    assert(isa<StructType>(type));
    if (dyn_cast<StructType>(type)->isLiteral()){
      dbgs() << "A literal struct has FRGs: \n";
    }
    else{
      dbgs() << "Struct [" << type->getStructName() << "] has FRGs: \n";
    }
    dbgs().changeColor(raw_ostream::GREEN);
    it.second->debugPrintFieldReferenceGraph(dbgs());
    dbgs().resetColor();
  }
  dbgs() << "----------------------------------------------------------- \n";
}

void StructFieldAccessManager::debugPrintAnnotatedModule() const
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
  }
  outs().resetColor();
  outs().changeColor(raw_ostream::GREEN);
  outs() << "Stats:\n";
  for (auto &it : StructFieldAccessInfoMap){
    for (auto i = 0; i < Stats::max_stats; i++){
      StatCounts[i] += it.second->getStats(i);
      if (i == Stats::gep_in_unknown){
        it.second->printUnknownOpcodes(outs());
      }
    }
  }
  for (auto i = 0; i < Stats::max_stats; i++){
    if (StatCounts[i]){
      outs() << "Case " << StatNames[i] << " was found " << StatCounts[i] <<  " times\n";
    }
  };
  FILE_OS << "GEP as Arg," << StatCounts[Stats::gep_in_arg] << "\n";
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
      auto* structInfoPtr = StructManager->createOrGetStructFieldAccessInfo(G.getValueType());
      assert(structInfoPtr);
      structInfoPtr->analyzeUsersOfStructValue(&G);
    }
    // Case for struct*
    else if (G.getValueType()->isPointerTy() && G.getValueType()->getPointerElementType()->isStructTy()){
      DEBUG_WITH_TYPE(DEBUG_TYPE_IR, dbgs() << "Found a global has struct* type: " << G << "\n");
      auto* structInfoPtr = StructManager->createOrGetStructFieldAccessInfo(G.getValueType()->getPointerElementType());
      assert(structInfoPtr);
      structInfoPtr->analyzeUsersOfStructPointerValue(&G);
      //StructManager->addStats(StructFieldAccessManager::Stats::struct_pointer);
    }
    // Case for struct**
    else if (G.getType()->isPointerTy() && G.getType()->getPointerElementType()->isPointerTy() && G.getType()->getPointerElementType()->getPointerElementType()->isStructTy()){
      DEBUG_WITH_TYPE(DEBUG_TYPE_IR, dbgs() << "Found a global has struct** type: " << G << " but we ignored this\n");
      StructManager->addStats(StructFieldAccessManager::Stats::struct_pointer_pointer);
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
            auto* structInfoPtr = StructManager->createOrGetStructFieldAccessInfo(type);
            structInfoPtr->analyzeUsersOfStructValue(&I);
          }
          else if (type->isPointerTy() && type->getPointerElementType()->isStructTy()){
            DEBUG_WITH_TYPE(DEBUG_TYPE_IR, dbgs() << "Found an alloca of a struct*: " << I << "\n");
            auto* structInfoPtr = StructManager->createOrGetStructFieldAccessInfo(type->getPointerElementType());
            assert(structInfoPtr);
            structInfoPtr->analyzeUsersOfStructPointerValue(&I);
            //StructManager->addStats(StructFieldAccessManager::Stats::struct_pointer);
          }
          else if (type->isPointerTy() && type->getPointerElementType()->isPointerTy() && type->getPointerElementType()->getPointerElementType()->isStructTy()){
            DEBUG_WITH_TYPE(DEBUG_TYPE_IR, dbgs() << "Found an alloca of a struct**: " << I << " but we ignore this\n");
            StructManager->addStats(StructFieldAccessManager::Stats::struct_pointer_pointer);
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
        StructManager->addStats(StructFieldAccessManager::Stats::func_arg_value);
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
          StructManager->addStats(StructFieldAccessManager::Stats::func_arg_not_defined);
        }
      }
    }
  }
  DEBUG(StructManager->debugPrintAllStructAccesses());
  DEBUG(StructManager->debugPrintAnnotatedModule());
  StructManager->printStats();
}

static void createFieldReferenceGraph(StructFieldAccessManager* StructManager)
{
  //FIXME: add a filter to filter out structs to ignore
  StructManager->buildFieldReferenceGraphForAllStructs();
  DEBUG(StructManager->debugPrintAllFRGs());
}

static bool performStructFieldCacheAnalysis(Module &M,
                                            function_ref<BlockFrequencyInfo *(Function &)> LookupBFI,
                                            function_ref<BranchProbabilityInfo *(Function &)> LookupBPI)
{
  DEBUG(dbgs() << "Start of struct field cache analysis\n");
  StructFieldAccessManager StructManager(M, LookupBFI, LookupBPI);
  // Step 0 - retrieve debug info for all struct FIXME: disable for now because it's not supporting annonymous structs
  //StructManager.retrieveDebugInfoForAllStructs();
  // Step 1 - perform IR analysis to collect info of all structs
  performIRAnalysis(M, &StructManager);
  // Step 2 - create Field Refernce Graph according to the results of IR analysis
  createFieldReferenceGraph(&StructManager);

  DEBUG(dbgs() << "End of struct field cache analysis\n");
  return false;
}

StructFieldCacheAnalysis::StructFieldCacheAnalysis() {}

PreservedAnalyses StructFieldCacheAnalysis::run(Module &M, ModuleAnalysisManager &AM) {
  auto &FAM = AM.getResult<FunctionAnalysisManagerModuleProxy>(M).getManager();
  auto LookupBFI = [&FAM](Function& F) {
    return &FAM.getResult<BlockFrequencyAnalysis>(F);
  };
  auto LookupBPI = [&FAM](Function& F) {
    return &FAM.getResult<BranchProbabilityAnalysis>(F);
  };
  if (!performStructFieldCacheAnalysis(M, LookupBFI, LookupBPI))
    return PreservedAnalyses::all();
  return PreservedAnalyses::none();
}


bool StructFieldCacheAnalysisPass::runOnModule(Module &M){
  auto LookupBFI = [this](Function& F) {
    return &this->getAnalysis<BlockFrequencyInfoWrapperPass>(F).getBFI();
  };
  auto LookupBPI = [this](Function& F) {
    return &this->getAnalysis<BranchProbabilityInfoWrapperPass>(F).getBPI();
  };
  return performStructFieldCacheAnalysis(M, LookupBFI, LookupBPI);
}
