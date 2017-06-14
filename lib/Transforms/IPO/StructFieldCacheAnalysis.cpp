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

#include <queue>
#include <unordered_set>
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
class FieldReferenceGraph
{
 public:
  struct Edge;
  struct Node{
    Node(unsigned I, unsigned N): Id(I), FieldNum(N) {}
    unsigned Id;
    unsigned FieldNum;
    std::unordered_set<Edge*> InEdges;
    std::unordered_set<Edge*> OutEdges;
  };

  struct Edge{
   public:
    Edge(unsigned I, unsigned C, unsigned D): Id(I), ExecutionCount(C), DataSize(D){}
    void connectNodes(Node* From, Node* To) { FromNode = From; ToNode = To; }
    unsigned Id;
    unsigned ExecutionCount;
    unsigned DataSize;
    Node* FromNode;
    Node* ToNode;
  };

  struct BasicBlockHelperInfo{
    BasicBlockHelperInfo(): RemainBytes(0), FirstNode(NULL), LastNode(NULL) {}
    unsigned RemainBytes;
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
  Node* createNewNode(unsigned FieldNum);
  Node* createNewNode() { return createNewNode(0); }

  // The two functions are used to connect two nodes in FRG with or without given weight
  void connectNodes(Node* From, Node* To, unsigned C, unsigned D);
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
  StructFieldAccessManager(const Module& M, function_ref<BlockFrequencyInfo *(Function &)> L):
      CurrentModule(M), LookupBFI(L), StatCounts(stats::max_stats) {};
  ~StructFieldAccessManager();
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
  // Retrive a pair of information if the instruction is accessing any struct type and field number
  Optional<std::pair<const Type*, unsigned> > getFieldAccessOnInstruction(const Instruction* I) const;

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
};

class StructFieldAccessInfo
{
  /* This class is used to store all access information for each struct declared in
    the program. It records all loads and stores to all fields of the struct to provide
    essential information for cache-aware struct field analysis.
  */
 public:
  StructFieldAccessInfo(const Type* T, const StructFieldAccessManager* SM, const Module& M): StructureType(dyn_cast<StructType>(T)), NumElements(StructureType->getNumElements()), StructManager(SM), CurrentModule(M), StatCounts(StructFieldAccessManager::stats::max_stats) {}
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

  // Functions for building FRG
  void buildFieldReferenceGraph();
  void buildFieldReferenceGraph(const Function* FOA);

  // Print all instructions that access any struct field
  void debugPrintAllStructAccesses(raw_ostream& OS) const;
  // Print all FRGs for this struct
  void debugPrintFieldReferenceGraph(raw_ostream& OS) const;

  // For stats
  void addStats(unsigned Category) { StatCounts[Category]++; }
  unsigned getStats(unsigned Category) const { return StatCounts[Category]; }

 private:
  const StructType* StructureType;
  unsigned NumElements;
  const StructFieldAccessManager* StructManager;
  const Module& CurrentModule;
  // A map records all instructions accessing which field of the structure
  std::unordered_map<const Instruction*, unsigned> FieldAccessMap;
  // A vector stores functions that contains struct accesses for further analysis
  std::unordered_set<const Function*> FunctionsForAnalysis;
  // A vector stores all FRGs for all functions
  std::vector<FieldReferenceGraph*> FRGArray;
  //A vector stores per struct stats
  std::vector<unsigned> StatCounts;

  // Private functions
  // Calculate which field of the struct is the GEP pointing to, from GetElementPtrInst or GEPOperator
  unsigned calculateFieldNumFromGEP(const User* U) const;
  // Record all users of a GEP instruction/operator that calculates the address of a field.
  // It's the only supported way to add access to a field for now
  void addFieldAccessFromGEP(const User* U);
  // Record an access pattern in the data structure
  void addFieldAccessNum(const Instruction* I, unsigned FieldNum);
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
FieldReferenceGraph::Node* FieldReferenceGraph::createNewNode(unsigned FieldNum)
{
  auto* Node = new FieldReferenceGraph::Node(NodeList.size(), FieldNum);
  NodeList.push_back(Node);
  return Node;
}
void FieldReferenceGraph::connectNodes(FieldReferenceGraph::Node* From, FieldReferenceGraph::Node* To, unsigned C, unsigned D)
{
  auto* Edge = new FieldReferenceGraph::Edge(EdgeList.size(), C, D);
  EdgeList.push_back(Edge);
  Edge->connectNodes(From, To);
  From->OutEdges.insert(Edge);
  To->InEdges.insert(Edge);
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
void StructFieldAccessInfo::addFieldAccessNum(const Instruction* I, unsigned FieldNum)
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

void StructFieldAccessInfo::buildFieldReferenceGraph(const Function* F)
{
  DEBUG(dbgs() << "Create a new empty FRG\n");
  auto* FRG = new FieldReferenceGraph(F);
  FRGArray.push_back(FRG);
  // Create and connect node inside each basic block
  for (auto &BB : *F){
    DEBUG(dbgs() << "Build partial FRG for BB: " << BB << "\n");
    auto* BBI = FRG->createBasicBlockHelperInfo(&BB);
    for (auto &I : BB){
      if (auto FieldNum = getAccessFieldNum(&I)){
        // Case that I is a struct access
        DEBUG(dbgs() << "Found an instruction " << I << " is a struct access on field [" << FieldNum.getValue() << "]\n");
        auto* NewNode = FRG->createNewNode(FieldNum.getValue());
        if (BBI->LastNode){
          DEBUG(dbgs() << "Previous nodes found in the BB\n");
          auto C = 0;
          if (auto ExCnt = getExecutionCount(&I))
            C = getExecutionCount(&I).getValue();
          auto D = BBI->RemainBytes;
          FRG->connectNodes(BBI->LastNode, NewNode, C, D);
          BBI->RemainBytes = 0;
          DEBUG(dbgs() << "Connect new node with previous node in the BB: Field [" << BBI->LastNode->FieldNum << "] to [" << FieldNum.getValue() << "] with count: " << C << " and data " << D << " bytes \n");
          BBI->LastNode = NewNode;
        }
        else{
          DEBUG(dbgs() << "No previous node found. It is the first node\n");
          BBI->FirstNode = BBI->LastNode = NewNode;
        }
      }
      else{
        // Case that I is not struct access but a memory access
        if (I.getOpcode() == Instruction::Load || I.getOpcode() == Instruction::Store){
          DEBUG(dbgs() << "Found an instruction " << I << " is not a struct access but a load/store.\n");
          if (BBI->LastNode == NULL){
            // Create a dummy node for the first non-struct memory access
            DEBUG(dbgs() << "Create a dummy node as the first node in the BB.\n");
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
            DEBUG(dbgs() << "Increment remaining data size to " << BBI->RemainBytes << "\n");
          }
        }
      }
    }
    if (BBI->LastNode == NULL){
      assert(BBI->FirstNode == NULL);
      DEBUG(dbgs() << "Create a dummy node as BB does not have memory accesses\n");
      BBI->FirstNode = BBI->LastNode = FRG->createNewNode();
    }
  }
  // Connect nodes between different basic blocks
  for (auto &BB : *F){
    auto* BBI = FRG->getBasicBlockHelperInfo(&BB);
    auto* Term = BB.getTerminator();
    for (const auto *SB : Term->successors()){
      auto* SBI = FRG->getBasicBlockHelperInfo(SB);
      unsigned C = 0;
      auto BBCount = getExecutionCount(&BB), SBCount = getExecutionCount(SB);
      if (BBCount.hasValue() && SBCount.hasValue())
        C = std::min(BBCount.getValue(), SBCount.getValue());
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
    outs() << "Gonna build FRG for Function " << F->getName() << "\n";
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
StructFieldAccessInfo* StructFieldAccessManager::createOrGetStructFieldAccessInfo(const Type* T)
{
  if (auto* def = getStructFieldAccessInfo(T))
    return def;
  else{
    assert(T->isStructTy() && isa<StructType>(T));
    def = StructFieldAccessInfoMap[T] = new StructFieldAccessInfo(T, this, CurrentModule);
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
  outs() << "------------ Printing stats for struct accesses: ---------------- \n";
  outs().changeColor(raw_ostream::YELLOW);
  outs() << "There are " << StructFieldAccessInfoMap.size() << " struct types are accessed in the program\n";
  for (auto &it : StructFieldAccessInfoMap){
    auto* type = it.first;
    assert(isa<StructType>(type));
    if (dyn_cast<StructType>(type)->isLiteral()){
      outs() << "A literal struct has " << it.second->getTotalNumFieldAccess() << " accesses.\n";
    }
    else{
      outs() << "Struct [" << type->getStructName() << "] has " << it.second->getTotalNumFieldAccess() << " accesses.\n";
    }
    for (auto i = 0; i < stats::max_stats; i++){
      StatCounts[i] += it.second->getStats(i);
    }
  }
  outs().resetColor();
  outs().changeColor(raw_ostream::GREEN);
  outs() << "Stats:\n";
  for (auto i = 0; i < stats::max_stats; i++){
    if (StatCounts[i])
      outs() << "Case " << StatNames[i] << " was found " << StatCounts[i] <<  "times\n";
  };
  outs().resetColor();
  outs() << "----------------------------------------------------------------- \n";
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
      auto* structInfoPtr = StructManager->createOrGetStructFieldAccessInfo(G.getValueType());
      assert(structInfoPtr);
      structInfoPtr->analyzeUsersOfStructValue(&G);
    }
    // Case for struct*
    else if (G.getValueType()->isPointerTy() && G.getValueType()->getPointerElementType()->isStructTy()){
      DEBUG(dbgs() << "Found a global has struct* type: " << G << "\n");
      auto* structInfoPtr = StructManager->createOrGetStructFieldAccessInfo(G.getValueType()->getPointerElementType());
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
            auto* structInfoPtr = StructManager->createOrGetStructFieldAccessInfo(type);
            structInfoPtr->analyzeUsersOfStructValue(&I);
          }
          else if (type->isPointerTy() && type->getPointerElementType()->isStructTy()){
            DEBUG(dbgs() << "Found an alloca of a struct*: " << I << "\n");
            auto* structInfoPtr = StructManager->createOrGetStructFieldAccessInfo(type->getPointerElementType());
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

static void createFieldReferenceGraph(StructFieldAccessManager* StructManager)
{
  //FIXME: add a filter to filter out structs to ignore
  StructManager->buildFieldReferenceGraphForAllStructs();
  DEBUG(StructManager->debugPrintAllFRGs());
}

static bool performStructFieldCacheAnalysis(Module &M,
                                            function_ref<BlockFrequencyInfo *(Function &)> LookupBFI)
{
  DEBUG(dbgs() << "Start of struct field cache analysis\n");
  StructFieldAccessManager StructManager(M, LookupBFI);
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
