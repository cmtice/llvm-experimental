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
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/IR/AssemblyAnnotationWriter.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Operator.h"
#include "llvm/Pass.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/IPO.h"

#include <cmath>
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
#define DEBUG_TYPE_CPG "struct-analysis-CPG"
#define DEBUG_TYPE_CPG_BF "struct-analysis-CPG-brutal-force"
//#define DEBUG_TYPE_CPG "struct-analysis"

#define DEBUG_PRINT_COUNT(x) (format("%.2f", (x)))
#define DEBUG_PRINT_DIST(x) (format("%.3f", (x)))

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
    AU.addRequired<LoopInfoWrapperPass>();
  }
};
}

char StructFieldCacheAnalysisPass::ID = 0;
INITIALIZE_PASS_BEGIN(StructFieldCacheAnalysisPass, "struct-field-cache-analysis", "Struct Field Cache Analysis", false, false)
INITIALIZE_PASS_DEPENDENCY(BlockFrequencyInfoWrapperPass)
INITIALIZE_PASS_DEPENDENCY(BranchProbabilityInfoWrapperPass)
INITIALIZE_PASS_DEPENDENCY(LoopInfoWrapperPass)
INITIALIZE_PASS_END(StructFieldCacheAnalysisPass, "struct-field-cache-analysis", "Struct Field Cache Analysis", false, false)
ModulePass *llvm::createStructFieldCacheAnalysisPass() { return new StructFieldCacheAnalysisPass; }

namespace llvm{
//typedef unsigned ExecutionCountType;
typedef double ExecutionCountType;
//typedef unsigned DataBytesType;
typedef double DataBytesType;
typedef unsigned FieldNumType;

class FieldReferenceGraph
{
 public:
  struct Edge;
  struct Entry{
    Entry(unsigned I, FieldNumType N, ExecutionCountType C, DataBytesType D) : Id(I), FieldNum(N), ExecutionCount(C), DataSize(D) {}
    unsigned Id;
    FieldNumType FieldNum;
    ExecutionCountType ExecutionCount;
    DataBytesType DataSize;
  };
  struct Node{
    Node(unsigned I, FieldNumType N, DataBytesType S): Id(I), FieldNum(N), Size(S), Visited(false), InSum(0), OutSum(0) {}
    unsigned Id;
    FieldNumType FieldNum;
    DataBytesType Size;
    bool Visited;
    ExecutionCountType InSum;
    ExecutionCountType OutSum;
    std::unordered_set<Edge*> InEdges;
    std::unordered_set<Edge*> OutEdges;
  };

  struct Edge{
   public:
    Edge(unsigned I, ExecutionCountType C, DataBytesType D): Id(I), ExecutionCount(C), DataSize(D), Collapsed(false), LoopArc(false) {}
    void connectNodes(Node* From, Node* To) { FromNode = From; ToNode = To; }
    void reconnect(Node* From, Node* To);
    unsigned Id;
    ExecutionCountType ExecutionCount;
    DataBytesType DataSize;
    bool Collapsed;
    bool LoopArc;
    Node* FromNode;
    Node* ToNode;
    // For Collpased Nodes
    std::unordered_set<Entry*> CollapsedEntries;
  };

  struct BasicBlockHelperInfo{
    BasicBlockHelperInfo(): RemainBytes(0), FirstNode(NULL), LastNode(NULL) {}
    DataBytesType RemainBytes;
    Node* FirstNode;
    Node* LastNode;
  };

 public:
  FieldReferenceGraph(const Function* F) : Func(F), RootNode(NULL) {}
  ~FieldReferenceGraph() {
    for (auto *N : NodeList)
      delete N;
    for (auto *E : EdgeList)
      delete E;
    for (auto *E : EntryList)
      delete E;
    for (auto &it : BBInfoMap)
      delete it.second;
    NodeList.clear();
    EdgeList.clear();
    EntryList.clear();
    BBInfoMap.clear();
  }

  // Functions for building FRG
  // Creator and getter of helper info for the basic block, useful when connect nodes from different basic blocks
  BasicBlockHelperInfo* createBasicBlockHelperInfo(const BasicBlock* BB);
  BasicBlockHelperInfo* getBasicBlockHelperInfo(const BasicBlock* BB);

  // The two functions are used to create a new node in the graph, unconnected with other nodes, and return the pointer to the Node
  Node* createNewNode(FieldNumType FieldNum, DataBytesType S);
  Node* createNewNode() { return createNewNode(0, 0); }

  // The two functions are used to connect two nodes in FRG with or without given weight
  void connectNodes(Node* From, Node* To, ExecutionCountType C, DataBytesType D, bool BackEdge = false);
  void connectNodes(Node* From, Node* To) { connectNodes(From, To, 0, 0); }

  // The getter and setter of entry node in the FRG
  Node* getRootNode() const { return RootNode; }
  void setRootNode(Node* N) { RootNode = N; }

  // Functions for collapsing FRG
  // Convert a node to a collapsed entry and add it to Edge
  void collapseNodeToEdge(Node* N, Edge* E);
  // Copy a collapsed entry to a new edge
  void moveCollapsedEntryToEdge(Entry* Entry, Edge* FromEdge, Edge* ToEdge);
  unsigned getNumNodes() const { return NodeList.size(); }
  Node* getNodeById(unsigned Id) const { assert(Id < NodeList.size()); return NodeList[Id]; }

  // For debug
  void debugPrint(raw_ostream& OS) const;
  void checkInOutSum() const;
  void debugPrintCollapsedEntries(raw_ostream& OS, FieldReferenceGraph::Edge* E) const;

 private:
  const Function* Func;
  Node* RootNode;
  std::vector<Node*> NodeList;
  std::vector<Edge*> EdgeList;
  std::vector<Entry*> EntryList;
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
                           function_ref<BranchProbabilityInfo *(Function &)> LBPI,
                           function_ref<LoopInfo *(Function &)> LLI):
      CurrentModule(M),
      LookupBFI(LBFI),
      LookupBPI(LBPI),
      LookupLI(LLI),
      StatCounts(Stats::max_stats) {};
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
  void buildCloseProximityRelations();
  // Check if an edge from FromBB to ToBB is a back edge in any loop
  bool isBackEdgeInLoop(const BasicBlock* FromBB, const BasicBlock* ToBB) const;

  // Functions for debugging
  // Print all accesses of all struct types defined in the program
  void debugPrintAllStructAccesses() const;
  // Print all FRGs of all struct types
  void debugPrintAllFRGs() const;
  // Print all CPGs of all struct types
  void debugPrintAllCPGs() const;
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
  // Function reference that is used to calculate branch probability
  function_ref<BranchProbabilityInfo *(Function &)> LookupBPI;
  // Function reference that is used to retrive loop information
  function_ref<LoopInfo *(Function &)> LookupLI;
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
  StructFieldAccessInfo(const Type* T, const Module& MD, const StructFieldAccessManager* M, const DICompositeType* D): CurrentModule(MD), StructureType(dyn_cast<StructType>(T)), DebugInfo(D), NumElements(StructureType->getNumElements()), StructManager(M), StatCounts(StructFieldAccessManager::Stats::max_stats) {
    CloseProximityTable.resize(NumElements);
    DEBUG_WITH_TYPE(DEBUG_TYPE_CPG_BF, GoldCPT.resize(NumElements));
    for (unsigned i = 0; i < NumElements; i++){
      CloseProximityTable[i].resize(NumElements);
      DEBUG_WITH_TYPE(DEBUG_TYPE_CPG_BF, GoldCPT[i].resize(NumElements));
      for (unsigned j = 0; j < NumElements; j++){
        CloseProximityTable[i][j] = std::make_pair(0, 0);
        DEBUG_WITH_TYPE(DEBUG_TYPE_CPG_BF, GoldCPT[i][j] = std::make_pair(0, 0));
      }
    }
    outs() << "Struct Elements: " << NumElements << "\n";
    outs() << "CPG rows: " << CloseProximityTable.size() << " columns: " << CloseProximityTable[0].size() << "\n";
  }

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
  FieldReferenceGraph* buildFieldReferenceGraph(const Function* F);
  // Functions for building CPG
  void buildCloseProximityRelations();

  // Print all instructions that access any struct field
  void debugPrintAllStructAccesses(raw_ostream& OS) const;
  // Print all FRGs for this struct
  void debugPrintFieldReferenceGraph(raw_ostream& OS) const;
  // Print the CPG of this struct
  void debugPrintCloseProximityGraph(raw_ostream& OS) const;
  // Print the Gold CPG of this struct
  void debugPrintGoldCPT(raw_ostream& OS) const;

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
  // Close Proximity relation table
  std::vector< std::vector< std::pair<ExecutionCountType, DataBytesType> > > CloseProximityTable;
  // Golden Close Proximity relation table for debugging
  std::vector< std::vector< std::pair<ExecutionCountType, DataBytesType> > > GoldCPT;
  //A vector stores per struct stats
  std::vector<unsigned> StatCounts;
  std::unordered_set<unsigned> UnknownOpcodes;

  // Private functions
  // Functions for building FRG
  // Calculate which field of the struct is the GEP pointing to, from GetElementPtrInst or GEPOperator
  FieldNumType calculateFieldNumFromGEP(const User* U) const;
  // Record all users of a GEP instruction/operator that calculates the address of a field.
  // It's the only supported way to add access to a field for now
  void addFieldAccessFromGEP(const User* U);
  // Record an access pattern in the data structure
  void addFieldAccessNum(const Instruction* I, FieldNumType FieldNum);

  // Use debug info to remaping struct fields (not using yet)
  void remapFieldFromDebugInfo();
  // Calculate memory access data size in Bytes
  DataBytesType getMemAccessDataSize(const Instruction* I) const;

  // Functions for building CPG
  void updateCPG(FieldNumType Src, FieldNumType Dest, ExecutionCountType C, DataBytesType D);
  // Build CPG with brutal force
  void updateGoldCPG(FieldNumType Src, FieldNumType Dest, ExecutionCountType C, DataBytesType D);
  void calculateCPRelation(std::vector<FieldReferenceGraph::Edge*>* Path);
  void findPathInFRG(FieldReferenceGraph::Node* Start, FieldReferenceGraph::Node* End, std::vector<FieldReferenceGraph::Edge*>* Path, std::unordered_set<FieldReferenceGraph::Node*>* VisitedNodes);
  void calculatePathBetween(FieldReferenceGraph::Node* FromNode, FieldReferenceGraph::Node* ToNode);
  void createFRGPairs(FieldReferenceGraph* FRG, FieldReferenceGraph::Node* Root, std::vector<FieldReferenceGraph::Edge*>* Path);
  void createGoldCloseProximityRelations(FieldReferenceGraph* FRG);
  void compareCloseProximityRelations() const;
  void checkFRG() const;
  // Build CPG with collapsing FRG first
  void updateCPGBetweenNodes(FieldReferenceGraph::Node* From, FieldReferenceGraph::Node* To, FieldReferenceGraph::Edge* Edge, ExecutionCountType C, DataBytesType D, std::unordered_set<FieldReferenceGraph::Node*>* CheckList);
  void updateCPGFromNodeToSubtree(FieldReferenceGraph::Edge* E);
  bool collapseSuccessor(FieldReferenceGraph* FRG, FieldReferenceGraph::Edge* Arc);
  bool collapseRoot(FieldReferenceGraph* FRG, FieldReferenceGraph::Node* Root);
  void collapseFieldReferenceGraph(FieldReferenceGraph* FRG);
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

// Utility functions
static void updatePairByMerging(ExecutionCountType& ResultCount, DataBytesType& ResultDistance, ExecutionCountType SourceCount1, DataBytesType SourceDistance1, ExecutionCountType SourceCount2, DataBytesType SourceDistance2)
{
  ResultCount = SourceCount1 + SourceCount2;
  if (ResultCount != 0)
    ResultDistance = (SourceCount1 * SourceDistance1 + SourceCount2 * SourceDistance2) / ResultCount;
  else
    ResultDistance = 0;
}
static void updatePairByMerging(ExecutionCountType& ResultCount, DataBytesType& ResultDistance, ExecutionCountType SourceCount, DataBytesType SourceDistance)
{
  //  DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Before merging: " << DEBUG_PRINT_COUNT(ResultCount) << " and " << DEBUG_PRINT_DIST(ResultDistance));
  if (SourceCount + ResultCount != 0)
    ResultDistance = (SourceCount * SourceDistance + ResultCount * ResultDistance) / (SourceCount + ResultCount);
  else
    ResultDistance = 0;
  ResultCount += SourceCount;
  //DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "After merging: " << DEBUG_PRINT_COUNT(ResultCount) << " and " << DEBUG_PRINT_DIST(ResultDistance));
  //updatePairByMerging(ResultCount, ResultDistance, ResultCount, ResultDistance, SourceCount, SourceDistance);
}

static void updatePairByConnecting(ExecutionCountType& ResultCount, DataBytesType& ResultDistance, ExecutionCountType SourceCount, double Ratio, DataBytesType SourceDistance1, DataBytesType SourceDistance2)
{
  assert(Ratio <= 1);
  ResultCount = SourceCount * Ratio;
  ResultDistance = SourceDistance1 + SourceDistance2;
}

// Functions for FieldReferenceGraph
void FieldReferenceGraph::Edge::reconnect(FieldReferenceGraph::Node* From, FieldReferenceGraph::Node* To)
{
  DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Reconnect edge: (" << FromNode->Id << "," << ToNode->Id <<") to (" << From->Id << "," << To->Id << ") with " << DEBUG_PRINT_COUNT(ExecutionCount) << " and " << DEBUG_PRINT_DIST(DataSize) << "\n");
  if (From == To){
    DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Reconnection results in a node pointing to itself, remove this edge\n");
    FromNode->OutEdges.erase(this);
    ToNode->InEdges.erase(this);
    FromNode = NULL;
    ToNode = NULL;
    return;
  }
  assert(From != FromNode || To != ToNode); // Assume never reconnect two nodes already connected with this Edge
  Edge* ExistEdge = NULL;
  for (auto *E : From->OutEdges){
    if (E->ToNode == To)
      ExistEdge = E;
  }
  if (ExistEdge){
    assert(To->InEdges.find(ExistEdge) != To->InEdges.end());
    DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Node " << From->Id << " and Node " << To->Id << " already have an edge with " << DEBUG_PRINT_COUNT(ExistEdge->ExecutionCount) << " and " << DEBUG_PRINT_DIST(ExistEdge->DataSize) << "\n");
    // Reconnect edge to two nodes that have connection, weights need to be adjusted
    updatePairByMerging(ExecutionCount, DataSize, ExistEdge->ExecutionCount, ExistEdge->DataSize);
    /*
    ExecutionCount += ExistEdge->ExecutionCount;
    if (ExecutionCount != 0)
      DataSize = ((ExistEdge->ExecutionCount * ExistEdge->DataSize) + (ExecutionCount * DataSize)) / ExecutionCount;
    else
      DataSize = 0;
    */
    From->OutEdges.erase(ExistEdge);
    To->InEdges.erase(ExistEdge);
  }
  if (From != FromNode)
    FromNode->OutEdges.erase(this);
  if (To != ToNode)
    ToNode->InEdges.erase(this);
  From->OutEdges.insert(this);
  To->InEdges.insert(this);
  FromNode = From;
  ToNode = To;
  DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "After reconnection: (" << FromNode->Id << "," << ToNode->Id <<") with " << DEBUG_PRINT_COUNT(ExecutionCount) << " and " << DEBUG_PRINT_DIST(DataSize) << "\n");
}

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
FieldReferenceGraph::Node* FieldReferenceGraph::createNewNode(FieldNumType FieldNum, DataBytesType S)
{
  auto* Node = new FieldReferenceGraph::Node(NodeList.size(), FieldNum, S);
  NodeList.push_back(Node);
  return Node;
}

void FieldReferenceGraph::connectNodes(FieldReferenceGraph::Node* From, FieldReferenceGraph::Node* To, ExecutionCountType C, DataBytesType D, bool BackEdge)
{
  if (From == To){
    // Don't connect a node back to itself to avoid a node never be able to collapse
    return;
  }
  auto* Edge = new FieldReferenceGraph::Edge(EdgeList.size(), C, D);
  EdgeList.push_back(Edge);
  Edge->connectNodes(From, To);
  From->OutEdges.insert(Edge);
  To->InEdges.insert(Edge);
  From->OutSum += C;
  To->InSum += C;
  if (BackEdge)
    Edge->LoopArc = true;
}

void FieldReferenceGraph::collapseNodeToEdge(FieldReferenceGraph::Node* N, FieldReferenceGraph::Edge* E)
{
  assert(E->Collapsed == false);
  E->Collapsed = true;
  auto* Entry = new FieldReferenceGraph::Entry(EntryList.size(), N->FieldNum, E->ExecutionCount, E->DataSize);
  E->CollapsedEntries.insert(Entry);
  EntryList.push_back(Entry);
}

void FieldReferenceGraph::moveCollapsedEntryToEdge(FieldReferenceGraph::Entry* Entry, FieldReferenceGraph::Edge* FromEdge, FieldReferenceGraph::Edge* ToEdge)
{
  assert(FromEdge->Collapsed);
  ToEdge->CollapsedEntries.insert(Entry);
  //FromEdge->CollapsedEntries.erase(Entry);
}

void FieldReferenceGraph::checkInOutSum() const
{
  assert(RootNode);
  std::queue<Node*> ExamineList; // List of Nodes to check
  std::unordered_set<Node*> ExaminedSet; // Set of Nodes popped from list to avoid duplicate checking
  ExamineList.push(RootNode);
  while (!ExamineList.empty()){
    auto* Node = ExamineList.front();
    ExamineList.pop();
    if (ExaminedSet.find(Node) != ExaminedSet.end())
      continue;
    ExaminedSet.insert(Node);
    if (Node->InEdges.size() && Node->OutEdges.size() && std::abs(Node->InSum-Node->OutSum)/Node->InSum > 0.1){
      errs() << "Node " << Node->Id << " accessing field " << Node->FieldNum << " has in-sum " << Node->InSum << " and out-sum " << Node->OutSum << "\n";
      assert(0 && "Node in-sum not equal to out-sum");
    }
    for (auto* Edge : Node->OutEdges){
      ExamineList.push(Edge->ToNode);
    }
  }
}

void FieldReferenceGraph::debugPrint(raw_ostream& OS) const
{
  assert(RootNode);
  std::queue<Node*> ExamineList; // List of Nodes to print
  std::unordered_set<Node*> ExaminedSet; // Set of Nodes popped from list to avoid duplicate
  ExamineList.push(RootNode);
  OS << "Field Reference Graph for function: " << Func->getName() << "\n";
  while (!ExamineList.empty()){
    auto* Node = ExamineList.front();
    ExamineList.pop();
    if (ExaminedSet.find(Node) != ExaminedSet.end())
      continue;
    ExaminedSet.insert(Node);
    OS << "Node " << Node->Id << " accesses " << Node->FieldNum << " and has " << DEBUG_PRINT_COUNT(Node->OutSum) << " out sum and " << DEBUG_PRINT_COUNT(Node->InSum) << " in sum: ";
    OS << "connect with {";
    for (auto* Edge : Node->OutEdges){
      OS << " Node " << Edge->ToNode->Id << " (" << DEBUG_PRINT_COUNT(Edge->ExecutionCount) << "," << DEBUG_PRINT_DIST(Edge->DataSize) << ")  ";
      ExamineList.push(Edge->ToNode);
    }
    OS << "}\n";
  }
}

void FieldReferenceGraph::debugPrintCollapsedEntries(raw_ostream& OS, FieldReferenceGraph::Edge* E) const
{
  OS << "Collapsed entry for edge: (" << E->FromNode->Id << "," << E->ToNode->Id << ")\n";
  for (auto* CE : E->CollapsedEntries){
    OS << "Collapsed entry " << CE->Id << " field num: " << CE->FieldNum << ", count: " << DEBUG_PRINT_COUNT(CE->ExecutionCount) << ", distance: " << DEBUG_PRINT_DIST(CE->DataSize) << "\n";
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

DataBytesType StructFieldAccessInfo::getMemAccessDataSize(const Instruction* I) const
{
  assert(I->getOpcode() == Instruction::Load || I->getOpcode() == Instruction::Store);
  Type* type;
  if (I->getOpcode() == Instruction::Load)
    type = I->getType();
  else
    type = I->getOperand(0)->getType();
  assert (type->isSized());
  return CurrentModule.getDataLayout().getTypeSizeInBits(type) / 8;
}

FieldReferenceGraph* StructFieldAccessInfo::buildFieldReferenceGraph(const Function* F)
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
        auto* NewNode = FRG->createNewNode(FieldNum.getValue(), getMemAccessDataSize(&I));
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
            BBI->RemainBytes += getMemAccessDataSize(&I);
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
      if (C < 1e-3)
        continue;
      auto D = BBI->RemainBytes; // Use size of remaining data in BB
      if (StructManager->isBackEdgeInLoop(&BB, SB))
        FRG->connectNodes(BBI->LastNode, SBI->FirstNode, C, D, true);
      else
        FRG->connectNodes(BBI->LastNode, SBI->FirstNode, C, D);
    }
  }
  auto* BBI = FRG->getBasicBlockHelperInfo(&F->getEntryBlock());
  FRG->setRootNode(BBI->FirstNode);
  DEBUG(dbgs() << "---------- FRG for function " << F->getName() << "----------------\n");
  DEBUG(FRG->debugPrint(dbgs()));
  DEBUG(dbgs() << "------------------------------------------------------------------------\n");
  //FRG->checkInOutSum();
  return FRG;
}


void StructFieldAccessInfo::updateCPG(FieldNumType Src, FieldNumType Dest, ExecutionCountType C, DataBytesType D)
{
  assert(Src <= NumElements && Dest <= NumElements);
  if (Src == 0 || Dest == 0 || C < 1e-3 || Src == Dest)
    // Give up update if from or to a dummy node, or if the count is zero
    return;
  assert(!isnan(C));
  if (Src != 0 && Dest != 0){
    if (Src > Dest){
      std::swap(Src, Dest);
    }
    // Skip updates from/to a dummy node
    DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Gonna update CPG between " << Src << " and " << Dest << " with count " << DEBUG_PRINT_COUNT(C) << " and distance " << DEBUG_PRINT_DIST(D) << ":\n");
    DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << " Before update: " << "(" << DEBUG_PRINT_COUNT(CloseProximityTable[Src-1][Dest-1].first) << "," << DEBUG_PRINT_DIST(CloseProximityTable[Src-1][Dest-1].second) << ")\n");
    assert(CloseProximityTable[Src-1][Dest-1].first + C != 0);
    updatePairByMerging(CloseProximityTable[Src-1][Dest-1].first, CloseProximityTable[Src-1][Dest-1].second, C, D);
    /*
    CloseProximityTable[Src-1][Dest-1] = std::make_pair(
        ( CloseProximityTable[Src-1][Dest-1].first + C ) ,
        ( ( CloseProximityTable[Src-1][Dest-1].first * CloseProximityTable[Src-1][Dest-1].second +
            C * D ) / ( CloseProximityTable[Src-1][Dest-1].first + C ) )
                                                      );
    */
    if (isnan(CloseProximityTable[Src-1][Dest-1].first) || isnan(CloseProximityTable[Src-1][Dest-1].second)){
      errs() << "Update CPG between " << Src << " and " << Dest << " with count " << DEBUG_PRINT_COUNT(C) << " and distance " << DEBUG_PRINT_DIST(D) << "\n";
    }
    DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << " After update: " << "(" << DEBUG_PRINT_COUNT(CloseProximityTable[Src-1][Dest-1].first) << "," << DEBUG_PRINT_DIST(CloseProximityTable[Src-1][Dest-1].second) << ")\n");
        // Count = C_old + C_new
        // Distance = weighted distance of old and new
  }
}

void StructFieldAccessInfo::updateGoldCPG(FieldNumType Src, FieldNumType Dest, ExecutionCountType C, DataBytesType D)
{
  assert(Src <= NumElements && Dest <= NumElements);
  if (Src == 0 || Dest == 0 || C < 1e-3 || Src == Dest)
    return;
  if (Src != 0 && Dest != 0){
    if (Src > Dest){
      std::swap(Src, Dest);
    }
    DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Gonna update Gold CPG between " << Src << " and " << Dest << " with count " << DEBUG_PRINT_COUNT(C) << " and distance " << DEBUG_PRINT_DIST(D) << ":\n");
    DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << " Before update: " << "(" << DEBUG_PRINT_COUNT(GoldCPT[Src-1][Dest-1].first) << "," << DEBUG_PRINT_DIST(GoldCPT[Src-1][Dest-1].second) << ")\n");
    updatePairByMerging(GoldCPT[Src-1][Dest-1].first, GoldCPT[Src-1][Dest-1].second, C, D);
    /*
    GoldCPT[Src-1][Dest-1] = std::make_pair(
        ( GoldCPT[Src-1][Dest-1].first + C ) ,
        ( ( GoldCPT[Src-1][Dest-1].first * GoldCPT[Src-1][Dest-1].second +
            C * D ) / ( GoldCPT[Src-1][Dest-1].first + C ) )
            );*/
    DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << " After update: " << "(" << DEBUG_PRINT_COUNT(GoldCPT[Src-1][Dest-1].first) << "," << DEBUG_PRINT_DIST(GoldCPT[Src-1][Dest-1].second) << ")\n");
  }
}

void StructFieldAccessInfo::updateCPGBetweenNodes(FieldReferenceGraph::Node* From, FieldReferenceGraph::Node* To, FieldReferenceGraph::Edge* Arc, ExecutionCountType C, DataBytesType D, std::unordered_set<FieldReferenceGraph::Node*>* CheckList)
{
  if (To == NULL || To == From || CheckList->find(To) != CheckList->end())
    return;
  DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Update CPG from " << From->Id << " to " << To->Id << " with " << DEBUG_PRINT_COUNT(C) << " and " << DEBUG_PRINT_DIST(D) << "\n");
  assert(!isnan(C));
  updateCPG(From->FieldNum, To->FieldNum, C, D);
  CheckList->insert(To);
  for (auto* E : To->OutEdges){
    if (E->ToNode == From)
      continue;
    if (E->Collapsed){
      DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Update CPG from " << From->Id << " to collapsed subtree edge (" << E->FromNode->Id << "," << E->ToNode->Id << ") with ratio " << Arc->ExecutionCount/To->InSum << "\n");
      for (auto* Entry : E->CollapsedEntries){
        auto ExCnt = C;
        auto Dist = D;
        // FIXME: this doesn't feel right
        assert(To->InSum > 0);
        updatePairByConnecting(ExCnt, Dist, Entry->ExecutionCount, Arc->ExecutionCount/To->InSum, Dist, To->Size + Entry->DataSize);
        assert(!isnan(ExCnt));
        updateCPG(From->FieldNum, Entry->FieldNum, ExCnt, Dist);
      }
    }
    else{
      DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Found non-collapsed successor " << E->ToNode->Id << "\n");
      auto ExCnt = C;
      auto Dist = D;
      assert(To->OutSum > 0);
      updatePairByConnecting(ExCnt, Dist, ExCnt, E->ExecutionCount/To->OutSum, Dist, To->Size + E->DataSize);
      if (ExCnt > 1e-3)
        updateCPGBetweenNodes(From, E->ToNode, E, ExCnt, Dist, CheckList);
    }
  }
  CheckList->erase(To);
}

void StructFieldAccessInfo::updateCPGFromNodeToSubtree(FieldReferenceGraph::Edge* E)
{
  if (E->FromNode->FieldNum == 0 || E->ExecutionCount <= 1e-3)
    return;
  DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Update CPG from " << E->FromNode->Id << " to subtree " << E->ToNode->Id << "\n");
  std::unordered_set<FieldReferenceGraph::Node*> CheckList;
  updateCPGBetweenNodes(E->FromNode, E->ToNode, E, E->ExecutionCount, E->DataSize, &CheckList);
}

bool StructFieldAccessInfo::collapseSuccessor(FieldReferenceGraph* FRG, FieldReferenceGraph::Edge* Arc)
{
  // TODO: Why do this here?
  /*
  if (Arc->Collapsed){
    for (auto* Entry : Arc->CollapsedEntries){
      updateCPG(Arc->FromNode->FieldNum, Entry->FieldNum, Entry->ExecutionCount, Entry->DataSize);
      // TODO: Update ARC->src, ARC->dest
    }
  }
  */
  bool Ret = false;
  DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Collapse successor Edge (" << Arc->FromNode->Id << "," << Arc->ToNode->Id << ")\n");
  assert(!isnan(Arc->ExecutionCount));
  if (Arc->ToNode->FieldNum != 0){
    updateCPG(Arc->FromNode->FieldNum, Arc->ToNode->FieldNum, Arc->ExecutionCount, Arc->DataSize);
    FRG->collapseNodeToEdge(Arc->ToNode, Arc);
  }
  auto Edges = Arc->ToNode->OutEdges;
  assert(Arc->ToNode->InSum > 0);
  auto Ratio = Arc->ExecutionCount/Arc->ToNode->InSum;
  for (auto *E : Edges){
    assert(E->FromNode == Arc->ToNode);
    DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Check edge: ("  << E->FromNode->Id << "," << E->ToNode->Id << ")\n");
    if (E->Collapsed){
      for (auto* Entry : E->CollapsedEntries){
        DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Move collapsed entry " << Entry->Id << " from edge: (" << E->FromNode->Id << "," << E->ToNode->Id << ") to (" << Arc->FromNode->Id << "," << Arc->ToNode->Id << ") with a ratio of " << format("%.3f", Ratio) << "\n");
        Entry->ExecutionCount = std::min(Entry->ExecutionCount*Ratio, Arc->ExecutionCount);
        Entry->DataSize += Arc->DataSize + Arc->ToNode->Size;
        assert(!isnan(Entry->ExecutionCount));
        updateCPG(Arc->FromNode->FieldNum, Entry->FieldNum, Entry->ExecutionCount, Entry->DataSize);
        FRG->moveCollapsedEntryToEdge(Entry, E, Arc);
      }
    }
    else{
      updateCPGFromNodeToSubtree(E);
      E->ExecutionCount = std::min(Arc->ExecutionCount, E->ExecutionCount*Ratio);
      E->DataSize += Arc->DataSize + Arc->ToNode->Size;
      E->reconnect(Arc->FromNode, E->ToNode);
      Ret = true;
      if (Arc->ToNode->OutEdges.size() == 0)
        break;
    }
  }
  DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, FRG->debugPrint(dbgs()));
  DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, FRG->debugPrintCollapsedEntries(dbgs(), Arc));
  return Ret;
}

bool StructFieldAccessInfo::collapseRoot(FieldReferenceGraph* FRG, FieldReferenceGraph::Node* Root)
{
  DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Collapse Node " << Root->Id << " as root\n");
  bool change = true;
  bool SubtreeChange = false;
  while (change){
    change = false;
    for (auto *E : Root->OutEdges){
      if (!E->LoopArc)
        change |= collapseRoot(FRG, E->ToNode);
    }
    for (auto *E : Root->OutEdges){
      DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Check out edge: (" << E->FromNode->Id << "," << E->ToNode->Id << "): ");
      if (E->LoopArc){
        DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "It is a loop arc\n");
        // Update CPG with accesses within SUCC loop
        //change = true;
      }
      else if (E->ToNode->InEdges.size() == 1 && !E->Collapsed){ // if SUCC can be collapsed, i.e. only has one predecessor
        DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Found a successor can be collapsed\n");
        if (collapseSuccessor(FRG, E)){
          SubtreeChange = true;
          change = true;
        }
      }
      else{
        DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Successor cannot collapse and not a loop arc\n");
      }
    }
    if (change)
      DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Subtree changes, need to collapse again\n");
  }
  return SubtreeChange;
}

void StructFieldAccessInfo::collapseFieldReferenceGraph(FieldReferenceGraph* FRG)
{
  auto* Root = FRG->getRootNode();
  collapseRoot(FRG, Root);
}

void StructFieldAccessInfo::calculateCPRelation(std::vector<FieldReferenceGraph::Edge*>* Path)
{
  DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Calculate path: \n");
  for (auto* E: *Path){
    DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "(" << E->FromNode->Id << "," << E->ToNode->Id << ")" << ",");
  }
  DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "\n");
  assert(Path->size());
  auto C = (*Path)[0]->ExecutionCount;
  auto D = (*Path)[0]->DataSize;
  for (unsigned i = 1; i < Path->size(); i++){
    auto* E = (*Path)[i];
    updatePairByConnecting(C, D, C, E->ExecutionCount/E->FromNode->OutSum, D, E->DataSize + E->FromNode->Size);
  }
  updateGoldCPG((*Path)[0]->FromNode->FieldNum, (*Path)[Path->size()-1]->ToNode->FieldNum, C, D);
}

void StructFieldAccessInfo::findPathInFRG(FieldReferenceGraph::Node* Start, FieldReferenceGraph::Node* End, std::vector<FieldReferenceGraph::Edge*>* Path, std::unordered_set<FieldReferenceGraph::Node*>* VisitedNodes)
{
  for (auto* E : Start->OutEdges){
    if (E->ToNode == End){
      Path->push_back(E);
      DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Find the destination, gonna calculate\n");
      calculateCPRelation(Path);
      Path->pop_back();
    }
    else if (VisitedNodes->find(E->ToNode) != VisitedNodes->end()) {
      DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Loop found, don\'t continue to find path\n");
      continue;
    }
    else if (E->ExecutionCount) {
      Path->push_back(E);
      VisitedNodes->insert(E->ToNode);
      findPathInFRG(E->ToNode, End, Path, VisitedNodes);
      Path->pop_back();
      VisitedNodes->erase(E->ToNode);
    }
  }
}

void StructFieldAccessInfo::calculatePathBetween(FieldReferenceGraph::Node* FromNode, FieldReferenceGraph::Node* ToNode)
{
  std::vector<FieldReferenceGraph::Edge*> Path;
  std::unordered_set<FieldReferenceGraph::Node*> VisitedNodes;
  DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Calculate Path between " << FromNode->Id << " and " << ToNode->Id << "\n");
  assert(FromNode != ToNode);
  VisitedNodes.insert(FromNode);
  findPathInFRG(FromNode, ToNode, &Path, &VisitedNodes);
}

void StructFieldAccessInfo::createFRGPairs(FieldReferenceGraph* FRG, FieldReferenceGraph::Node* Root, std::vector<FieldReferenceGraph::Edge*>* Path)
{
  for (unsigned i = 0; i < FRG->getNumNodes(); i++){
    auto* FromNode = FRG->getNodeById(i);
    if (FromNode->FieldNum == 0) // Skip dummy nodes
      continue;
    for (unsigned j = 0; j < FRG->getNumNodes(); j++){
      if (i == j)
        continue;
      auto* ToNode = FRG->getNodeById(j);
      if (ToNode->FieldNum == 0) // Skip dummy nodes
        continue;
      if (FromNode->FieldNum == ToNode->FieldNum)
        continue;
      calculatePathBetween(FromNode, ToNode);
    }
  }
}

void StructFieldAccessInfo::createGoldCloseProximityRelations(FieldReferenceGraph* FRG)
{
  auto* Root = FRG->getRootNode();
  std::vector<FieldReferenceGraph::Edge*> Path;
  DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Create CP Relations for FRG with brutal force\n");
  createFRGPairs(FRG, Root, &Path);
}

void StructFieldAccessInfo::compareCloseProximityRelations() const{
  for (unsigned i = 0; i < NumElements; i++){
    for (unsigned j = i+1; j < NumElements; j++){
      if (std::abs(GoldCPT[i][j].first - CloseProximityTable[i][j].first) / GoldCPT[i][j].first > 0.1 ||
          std::abs(GoldCPT[i][j].second - CloseProximityTable[i][j].second) / GoldCPT[i][j].second > 0.1){
        outs() << "Found error in CPG: F" << i+1 << " and F" << j+1 << " should be (" << DEBUG_PRINT_COUNT(GoldCPT[i][j].first) << "," << DEBUG_PRINT_DIST(GoldCPT[i][j].second) << ") but calculated as (" << DEBUG_PRINT_COUNT(CloseProximityTable[i][j].first) << "," << DEBUG_PRINT_DIST(CloseProximityTable[i][j].second) << ")\n";
      }
    }
  }
}

void StructFieldAccessInfo::buildFieldReferenceGraph()
{
  for (auto *F : FunctionsForAnalysis){
    buildFieldReferenceGraph(F);
  }
}

void StructFieldAccessInfo::buildCloseProximityRelations()
{
  for (auto *F : FunctionsForAnalysis){
    DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Analyzing function " << F->getName() << "\n");
    auto* FRG = buildFieldReferenceGraph(F);
    assert(FRG);
    // Collpase FRG and get CPG
    collapseFieldReferenceGraph(FRG);
    // Brutal force get CP relations
    DEBUG_WITH_TYPE(DEBUG_TYPE_CPG_BF, FRG = buildFieldReferenceGraph(F));
    DEBUG_WITH_TYPE(DEBUG_TYPE_CPG_BF, createGoldCloseProximityRelations(FRG));
  }
  DEBUG_WITH_TYPE(DEBUG_TYPE_CPG_BF, compareCloseProximityRelations());
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

void StructFieldAccessInfo::debugPrintCloseProximityGraph(raw_ostream& OS) const
{
  for (unsigned i = 0; i < NumElements; i++){
    OS << "F" << i+1 << ": ";
    for (unsigned j = 0; j < NumElements; j++){
      if (j <= i)
        OS << "-\t";
      else
        OS << "(" << DEBUG_PRINT_COUNT(CloseProximityTable[i][j].first) << "," << DEBUG_PRINT_DIST(CloseProximityTable[i][j].second) << ")\t";
    }
    OS << "\n";
  }
}

void StructFieldAccessInfo::debugPrintGoldCPT(raw_ostream& OS) const
{
  for (unsigned i = 0; i < NumElements; i++){
    OS << "F" << i+1 << ": ";
    for (unsigned j = 0; j < NumElements; j++){
      if (j <= i)
        OS << "-\t";
      else
        OS << "(" << DEBUG_PRINT_COUNT(GoldCPT[i][j].first) << "," << DEBUG_PRINT_DIST(GoldCPT[i][j].second) << ")\t";
    }
    OS << "\n";
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

bool StructFieldAccessManager::isBackEdgeInLoop(const BasicBlock* FromBB, const BasicBlock* ToBB) const
{
  assert(FromBB->getParent() == ToBB->getParent());
  auto* LoopInfo = LookupLI(*const_cast<Function*>(FromBB->getParent()));
  assert(LoopInfo);
  auto* Loop1 = LoopInfo->getLoopFor(FromBB);
  auto* Loop2 = LoopInfo->getLoopFor(ToBB);
  // If FromBB and ToBB are not in the same loop, it can't be a backedge
  if (Loop1 != Loop2)
    return false;
  if (Loop1 == NULL)
    return false;
  // If ToBB is not the header of the loop, it can't be a backedge
  if (ToBB != Loop1->getHeader())
    return false;

  return true;
}

void StructFieldAccessManager::buildCloseProximityRelations()
{
  for (auto& it : StructFieldAccessInfoMap){
    if (it.second->getTotalNumFieldAccess() != 0){
      it.second->buildCloseProximityRelations();
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

void StructFieldAccessManager::debugPrintAllCPGs() const
{
  dbgs() << "------------ Printing all CPGs: ------------------- \n";
  for (auto &it : StructFieldAccessInfoMap){
    dbgs().changeColor(raw_ostream::YELLOW);
    auto* type = it.first;
    assert(isa<StructType>(type));
    if (dyn_cast<StructType>(type)->isLiteral()){
      dbgs() << "A literal struct has CPG: \n";
    }
    else{
      dbgs() << "Struct [" << type->getStructName() << "] has FRG: \n";
    }
    dbgs().changeColor(raw_ostream::GREEN);
    it.second->debugPrintCloseProximityGraph(dbgs());
    DEBUG_WITH_TYPE(DEBUG_TYPE_CPG_BF, it.second->debugPrintGoldCPT(dbgs()));
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

static void collapseFieldReferenceGraphAndCreateCloseProximityGraph(StructFieldAccessManager* StructManager)
{
  StructManager->buildCloseProximityRelations();
  DEBUG(StructManager->debugPrintAllCPGs());
}

static bool performStructFieldCacheAnalysis(Module &M,
                                            function_ref<BlockFrequencyInfo *(Function &)> LookupBFI,
                                            function_ref<BranchProbabilityInfo *(Function &)> LookupBPI,
                                            function_ref<LoopInfo *(Function &)> LookupLI)
{
  DEBUG(dbgs() << "Start of struct field cache analysis\n");
  StructFieldAccessManager StructManager(M, LookupBFI, LookupBPI, LookupLI);
  // Step 0 - retrieve debug info for all struct FIXME: disable for now because it's not supporting annonymous structs
  //StructManager.retrieveDebugInfoForAllStructs();
  // Step 1 - perform IR analysis to collect info of all structs
  performIRAnalysis(M, &StructManager);
  // Step 2 - create Field Refernce Graph according to the results of IR analysis
  //createFieldReferenceGraph(&StructManager); // FIXME: This step can be skipped if Step 3 is finalized
  // Step 3 - collapse Field Reference Graph and create Close Proximity Graph
  collapseFieldReferenceGraphAndCreateCloseProximityGraph(&StructManager);
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
  auto LookupLI = [&FAM](Function& F) {
    return &FAM.getResult<LoopAnalysis>(F);
  };
  if (!performStructFieldCacheAnalysis(M, LookupBFI, LookupBPI, LookupLI))
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
  auto LookupLI = [this](Function& F) {
    return &this->getAnalysis<LoopInfoWrapperPass>(F).getLoopInfo();
  };
  return performStructFieldCacheAnalysis(M, LookupBFI, LookupBPI, LookupLI);
}
