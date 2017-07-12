// lib/Tranforms/IPO/StructFieldCacheAnalysisImpl.h - Performs Cache-Aware
// Structure Analysis-*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===------------------------------------------------------------------------===//
//
// This pass performs analysis on cache-aware structure field accesses based on
// the following paper and reports recommendations on changes to make on the
// source code to improve performance.
//  [1] M. Hagog, C. Tice “Cache Aware Data Layout Reorganization Optimization
//  in GCC”, Proceedings
//      of the GCC Developers’ Summit,  Ottawa, 2005.
//
//===------------------------------------------------------------------------===//

#ifndef LLVM_TRANSFORM_IPO_STRUCTFIELDCACHEANALYSIS_IMPL_H
#define LLVM_TRANSFORM_IPO_STRUCTFIELDCACHEANALYSIS_IMPL_H

#include "llvm/Analysis/BlockFrequencyInfo.h"
#include "llvm/Analysis/BranchProbabilityInfo.h"
#include "llvm/IR/AssemblyAnnotationWriter.h"
#include "llvm/Pass.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/IPO/StructFieldCacheAnalysis.h"

#include <unordered_map>
#include <unordered_set>
#include <vector>

using namespace llvm;

#define DEBUG_TYPE "struct-analysis"
#define DEBUG_TYPE_IR "struct-analysis-IR"
#define DEBUG_TYPE_STATS "struct-analysis-detailed-stats"
#define DEBUG_TYPE_FRG "struct-analysis-FRG"
#define DEBUG_TYPE_CPG "struct-analysis-CPG"

namespace llvm {
typedef unsigned FieldNumType;
typedef std::pair<const StructType *, FieldNumType> StructInfoMapPairType;
typedef uint64_t ProfileCountType;
typedef std::vector<FieldNumType> FieldNumArrayType;
typedef double ExecutionCountType;
typedef double DataBytesType;
typedef std::unordered_set<const BasicBlock*> BasicBlockSetType;
typedef std::pair<ExecutionCountType, DataBytesType> CloseProximityPairType;
typedef std::vector< std::vector<CloseProximityPairType> > CloseProximityTableType;

class StructFieldAccessInfo;
class CloseProximityBuilder;

/// This class is used to analyze the hotness of each struct
class StructHotnessAnalyzer {
 public:
  StructHotnessAnalyzer() : MaxHotness(0) {}
  void addStruct(const StructFieldAccessInfo *SI);
  void generateHistogram();
  bool isHot(const StructFieldAccessInfo *SI) const;

 private:
  ProfileCountType MaxHotness;
  std::unordered_map<const StructType *, ProfileCountType> StructHotness;
  std::vector<unsigned> Histogram;
};

/// This class is used to keep track of all StructFieldAccessInfo objects
/// in the program and make sure only one StructFieldAccessInfo object for
/// each type of struct declared in the program.
class StructFieldAccessManager {
 public:
  /// enum used to represent different type of struct definitions
  enum StructDefinitionType {
    SDT_GlobalStruct,
    SDT_GlobalStructPtr,
    SDT_GlobalStructPtrPtr,
    SDT_LocalStruct,
    SDT_LocalStructPtr,
    SDT_LocalStructPtrPtr
  };

  /// enum used to count the corner cases in the program for future
  /// consideration
  enum DebugStats {
    DS_StructPtrPtr,
    DS_FuncArgValue,
    DS_FuncArgNotDefined,
    DS_GepPassedIntoIndirectFunc,
    DS_GepPassedIntoBitcast,
    DS_GepUnknownUse,
    DS_UserNotInstructionNorOperator,
    DS_NoAccess,
    DS_PassedIntoOutsideFunction,
    DS_GepUsedOnStructPtr,
    DS_UnknownUsesOnStructPtr,
    DS_FilterColdStructs,
    DS_MaxNumStats
  };

  StructFieldAccessManager(const Module &M,
                           function_ref<BlockFrequencyInfo *(Function &)> LBFI,
                           function_ref<BranchProbabilityInfo *(Function &)> LBPI)
      : CurrentModule(M), LookupBFI(LBFI), LookupBPI(LBPI),
        StatCounts(DebugStats::DS_MaxNumStats) {
    HotnessAnalyzer = new StructHotnessAnalyzer;
  };

  ~StructFieldAccessManager();

  /// Check if the struct type is created before; if not, create a new
  /// StructFieldAccessInfo object for it
  StructFieldAccessInfo *
  createOrGetStructFieldAccessInfo(const Type *T,
                                   const StructDefinitionType ST);

  /// Retrieve the pointer to the previous created StructFieldAccessInfo object
  /// for the type
  StructFieldAccessInfo *getStructFieldAccessInfo(const Type *T) const;

  /// Retrieve execution count for a basic block
  Optional<uint64_t> getExecutionCount(const BasicBlock *BB) const {
    Function *Func = const_cast<Function *>(BB->getParent());
    return LookupBFI(*Func)->getBlockProfileCount(BB);
  }

  /// Retrieve branch probability information of a branch
  Optional<double> getBranchProbability(const BasicBlock* FromBB, const BasicBlock* ToBB) const{
    assert(FromBB->getParent() == ToBB->getParent());
    Function* func = const_cast<Function*>(FromBB->getParent());
    auto Prob = LookupBPI(*func)->getEdgeProbability(FromBB, ToBB);
    return 1.0 * Prob.getNumerator() / Prob.getDenominator();
  }

  /// Retrive a pair of information if the instruction is accessing any struct
  /// type and field number
  Optional<StructInfoMapPairType>
  getFieldAccessOnInstruction(const Instruction *I) const;

  /// Summarizes all CallInst and InvokeInst into function declarations
  void summarizeFunctionCalls();
  /// Apply some filters to reduce the number of struct in analysis
  void applyFiltersToStructs();

  /// Build Close Proximity Graph for all structs in StructFieldAccessInfoMap
  void buildCloseProximityRelations();

  /// Print all accesses of all struct types defined in the program
  void debugPrintAllStructAccesses();

  /// Print all FRGs of all struct types
  void debugPrintAllFRGs() const;

  /// Print all CPGs of all struct types
  void debugPrintAllCPGs() const;

  /// Print the IR of the module with annotated information about struct access
  void debugPrintAnnotatedModule();

  /// Increment stats for one category
  void addStats(unsigned Category) { StatCounts[Category]++; }

  /// Print a brief stats of struct access
  void printStats();

 private:
  const Module &CurrentModule;

  /// Function reference that is used to retrive execution count for basic block
  function_ref<BlockFrequencyInfo *(Function &)> LookupBFI;

  /// Function reference that is used to calculate branch probability
  function_ref<BranchProbabilityInfo *(Function &)> LookupBPI;

  /// A map storing access info of all structs
  std::unordered_map<const StructType *, StructFieldAccessInfo *>
      StructFieldAccessInfoMap;

  /// A map storing Close Proximity relations of all structs
  std::unordered_map<const StructType *, CloseProximityBuilder *>
  CloseProximityBuilderMap;
  StructHotnessAnalyzer *HotnessAnalyzer;

  /// \name Data structure to get statistics of each DebugStats entry
  /// %{
  std::vector<unsigned> StatCounts;
  const std::vector<std::string> StatNames = {
      "Variable type is Struct**",
      "Function argument is a value",
      "Function argument is not defined in the program",
      "GEP value passed into indirect function calls or function that has "
      "undetermined num args",
      "GEP value passed into bitcast",
      "GEP value passed into unexpected opcode",
      "User is not Instruction nor Operator",
      "Struct defined but no accesses",
      "Struct passed into functions defined out of scope",
      "GEP instruction directly used on struct*",
      "Unknown instruction directly used on struct*",
      "Struct filtered out due to colder than a ratio of maximum hotness"};
  /// %}

  /// Used to print name of each StructDefinitionType
  const std::vector<std::string> StructDefinitionTypeNames = {
      "global struct", "global struct*", "global struct**",
      "local struct",  "local struct*",  "local struct**"};
};

/// This class is used to store all access information for each struct
/// declared in the program. It records all loads and stores to all fields
/// of the struct to provide essential information for cache-aware struct
/// field analysis.
class StructFieldAccessInfo {
 private:
  // This struct organizes a call on a function with each argument access which
  // struct field
  struct FunctionCallInfo {
    FunctionCallInfo(const Function *F, unsigned ArgNum, FieldNumType FieldNum)
        : FunctionDeclaration(F) {
      Arguments.resize(FunctionDeclaration->arg_size());
      assert(ArgNum < Arguments.size());
      Arguments[ArgNum] = FieldNum;
    }
    void insertCallInfo(unsigned ArgNum, FieldNumType FieldNum) {
      assert(ArgNum < Arguments.size());
      Arguments[ArgNum] = FieldNum;
    }
    const Function *FunctionDeclaration;
    FieldNumArrayType Arguments;
  };
  // This struct organizes all calls on a function definition with all mappings
  // of arguments and struct field number
  struct FunctionAccessPattern {
    FunctionAccessPattern(FieldNumArrayType *CallSite) {
      CallSites.clear();
      CallSites.push_back(CallSite);
    }
    void insertCallInfo(FieldNumArrayType *CallSite) {
      CallSites.push_back(CallSite);
    }
    std::vector<FieldNumArrayType *> CallSites;
  };

public:
  StructFieldAccessInfo(
      const StructType *ST,
      const StructFieldAccessManager::StructDefinitionType SDT,
      const Module &MD, const StructFieldAccessManager *M,
      const DICompositeType *D)
      : Eligiblity(true), CurrentModule(MD), StructureType(ST),
        StructDefinition(SDT), DebugInfo(D), NumElements(ST->getNumElements()),
        StructManager(M),
        StatCounts(StructFieldAccessManager::DebugStats::DS_MaxNumStats) {}

  ~StructFieldAccessInfo() {}

  const StructType *getStructType() const { return StructureType; }

  FieldNumType getNumElements() const { return NumElements; }

  StructFieldAccessManager::StructDefinitionType getStructDefinition() const {
    return StructDefinition;
  }
  bool isEligible() const { return Eligiblity; }

  /// Analyze a value pointing to a struct and collect struct access from it. It
  /// can be allocas/function args/globals
  void analyzeUsersOfStructValue(const Value *V);

  /// Analyze a value pointing to a struct* and collect struct access from it.
  /// It can be allocas/function args/globals
  void analyzeUsersOfStructPointerValue(const Value *V);

  /// Obtain which field the instruction is accessing and return no val if not
  /// accessing any struct field
  Optional<FieldNumType> getAccessFieldNum(const Instruction *I) const;

  /// Obtain total number of instructions that access the struct fields
  unsigned getTotalNumFieldAccess() const {
    return LoadStoreFieldAccessMap.size() + CallInstFieldAccessMap.size();
  }

  /// Obtain execution count for the BasicBlock/Instruction from profiling info,
  /// if any
  /// %{
  Optional<ProfileCountType> getExecutionCount(const BasicBlock *BB) const {
    return StructManager->getExecutionCount(BB);
  }
  Optional<ProfileCountType> getExecutionCount(const Instruction *I) const {
    return StructManager->getExecutionCount(I->getParent());
  }
  /// %}

  /// Summarize call/invoke uses of structs into function definitions
  void summarizeFunctionCalls();

  /// Calculate total hotness of all struct uses
  ProfileCountType calculateTotalHotness() const;

  /// Print all instructions that access any struct field
  void debugPrintAllStructAccesses(raw_ostream &OS);

  /// For stats
  /// %{
  void addStats(unsigned Category, unsigned Opcode = 0) {
    StatCounts[Category]++;
    if (Category == StructFieldAccessManager::DebugStats::DS_GepUnknownUse) {
      if (UnknownOpcodes.find(Opcode) == UnknownOpcodes.end())
        UnknownOpcodes[Opcode] = 0;
      else
        UnknownOpcodes[Opcode]++;
    }
  }
  unsigned getStats(unsigned Category) const { return StatCounts[Category]; }
  /// %}

  void printUnknownOpcodes(raw_ostream &OS) const {
    if (UnknownOpcodes.size() == 0)
      return;
    OS << "Unknown opcodes stats: \n";
    for (auto &it : UnknownOpcodes) {
      OS << "Opcode " << it.first << ": " << it.second << " times\n";
    }
  }

private:
  bool Eligiblity;
  const Module &CurrentModule;
  const StructType *StructureType;
  const StructFieldAccessManager::StructDefinitionType StructDefinition;
  const DICompositeType *DebugInfo;
  unsigned NumElements;
  const StructFieldAccessManager *StructManager;

  /// A map records all load/store instructions accessing which field of the
  /// structure
  std::unordered_map<const Instruction *, unsigned> LoadStoreFieldAccessMap;
  // A map records all call/invoke instructions accessing which field of the
  // structure
  std::unordered_map<const Instruction *, FunctionCallInfo *>
      CallInstFieldAccessMap;
  // A map records all functions that has calls with field accesses and their
  // calling patterns
  std::unordered_map<const Function *, FunctionAccessPattern *>
      FunctionAccessMap;
  // For stat
  std::vector<unsigned> StatCounts;
  std::unordered_map<unsigned, unsigned> UnknownOpcodes;

private:
  /// Calculate which field of the struct is the GEP pointing to, from
  /// GetElementPtrInst or GEPOperator
  FieldNumType calculateFieldNumFromGEP(const User *U) const;

  /// Record all users of a GEP instruction/operator that calculates the address
  /// of a field.
  void addFieldAccessFromGEP(const User *U);

  /// Record an access pattern in the data structure for a load/store
  void addFieldAccessNum(const Instruction *I, FieldNumType FieldNum);
  // Record an access pattern in the data structure for a call/invoke
  void addFieldAccessNum(const Instruction *I, const Function *F, unsigned Arg,
                         FieldNumType FieldNum);
};

/// This class is inherited from AssemblyAnnotationWriter and used
/// to print annotated information on IR
class StructFieldCacheAnalysisAnnotatedWriter
    : public AssemblyAnnotationWriter {
public:
  StructFieldCacheAnalysisAnnotatedWriter(
      const StructFieldAccessManager *S = NULL)
      : StructManager(S) {}

  // Override the base class function to print an annotate message after each
  // basic block
  virtual void emitBasicBlockEndAnnot(const BasicBlock *BB,
                                      formatted_raw_ostream &OS) {
    OS.resetColor();
    auto count = StructManager->getExecutionCount(BB);
    if (count.hasValue()) {
      OS.changeColor(raw_ostream::YELLOW, false, false);
      OS << "; [prof count = " << count.getValue() << "]\n";
      OS.resetColor();
    } else {
      OS.changeColor(raw_ostream::YELLOW, false, false);
      OS << "; [prof count not found "
         << "]\n";
      OS.resetColor();
    }
  }

  virtual void emitInstructionAnnot(const Instruction *I,
                                    formatted_raw_ostream &OS) {
    if (StructManager == NULL)
      return;
    if (auto pair = StructManager->getFieldAccessOnInstruction(I)) {
      OS.changeColor(raw_ostream::GREEN, false, false);
      auto *type = pair.getValue().first;
      if (type->isLiteral())
        OS << "; [Field " << pair.getValue().second
           << " of a literal struct.] ";
      else
        OS << "; [Field " << pair.getValue().second << " of struct "
           << type->getStructName() << "] ";
    } else {
      OS.resetColor();
    }
  }

 private:
  const StructFieldAccessManager *StructManager;
};

/// This class implements creation and organization of FieldReferenceGraph
class FieldReferenceGraph
{
 public:
  struct Edge;

  /// This structure represents a collapsed entry in collapsed FRG
  struct Entry{
    Entry(unsigned I, FieldNumType N, ExecutionCountType C, DataBytesType D) : Id(I), FieldNum(N), ExecutionCount(C), DataSize(D) {}
    unsigned Id;
    FieldNumType FieldNum;
    ExecutionCountType ExecutionCount;
    DataBytesType DataSize;
  };

  /// This structure represents a general node in FRG
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

  /// This structure represents an edge in FRG, before or after collapsing
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

  /// This structure stores nodes in each basic block and is used to help building FRG
  struct BasicBlockHelperInfo{
    BasicBlockHelperInfo(): RemainBytes(0), FirstNode(NULL), LastNode(NULL) {}
    DataBytesType RemainBytes;
    Node* FirstNode;
    Node* LastNode;
    BasicBlockSetType BackEdgeSet;
  };

  public:
  FieldReferenceGraph(const Function* F) : Func(F), RootNode(NULL) {
    NodeList.clear();
    EdgeList.clear();
    EntryList.clear();
    BBInfoMap.clear();
  }

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

  unsigned getNumNodes() const { return NodeList.size(); }

  Node* getNodeById(unsigned Id) const { assert(Id < NodeList.size()); return NodeList[Id]; }

  /// Functions for building FRG
  /// Creator and getter of helper info for the basic block, useful when connect nodes from different basic blocks
  /// %{
  BasicBlockHelperInfo* createBasicBlockHelperInfo(const BasicBlock* BB);
  BasicBlockHelperInfo* getBasicBlockHelperInfo(const BasicBlock* BB) const;
  /// %}

  /// The two functions are used to create a new node in the graph, unconnected with other nodes, and return the pointer to the Node
  /// %{
  Node* createNewNode(FieldNumType FieldNum, DataBytesType S);
  Node* createNewNode() { return createNewNode(0, 0); }
  /// %}

  /// The two functions are used to connect two nodes in FRG with or without given weight
  /// %{
  void connectNodes(Node* From, Node* To, ExecutionCountType C, DataBytesType D, bool BackEdge = false);
  void connectNodes(Node* From, Node* To) { connectNodes(From, To, 0, 0); }
  /// %}

  /// The getter and setter of entry node in the FRG
  /// %{
  Node* getRootNode() const { return RootNode; }
  void setRootNode(Node* N) { RootNode = N; }
  /// %}

  /// Functions for collapsing FRG
  /// Convert a node to a collapsed entry and add it to Edge
  void collapseNodeToEdge(Node* N, Edge* E);

  /// Copy a collapsed entry to a new edge
  void moveCollapsedEntryToEdge(Entry* Entry, Edge* FromEdge, Edge* ToEdge);

  /// For debug
  void debugPrint(raw_ostream& OS) const;
  void debugPrintCollapsedEntries(raw_ostream& OS, FieldReferenceGraph::Edge* E) const;

 private:
  const Function* Func;
  Node* RootNode;
  std::vector<Node*> NodeList;
  std::vector<Edge*> EdgeList;
  std::vector<Entry*> EntryList;
  std::unordered_map<const BasicBlock*, BasicBlockHelperInfo*> BBInfoMap;
};

typedef std::vector<FieldReferenceGraph::Edge*> EdgeArrayType;
typedef std::unordered_set<FieldReferenceGraph::Node*> NodeSetType;
typedef std::vector<FieldReferenceGraph*> FRGArrayType;

/// This class takes StructFieldAccessInfo and builds FieldReferenceGraph for each function of the struct.
/// It then collapses all FRGs to establish Close Proximity between each pair of fields of the struct.
class CloseProximityBuilder {

 public:
  CloseProximityBuilder(const Module& M, const StructFieldAccessManager* SM, const StructFieldAccessInfo* SI);

  ~CloseProximityBuilder() {
    for (auto* FRG : FRGArray){
      delete FRG;
    }
    FRGArray.clear();
  }

  /// Build FRG for a specified function by reading information from StructInfo
  FieldReferenceGraph* buildFieldReferenceGraph(const Function* F);

  /// Collapse FRG to establish CPG
  void buildCloseProximityRelations();

  /// Print all FRGs for this struct
  void debugPrintFieldReferenceGraph(raw_ostream& OS) const;
  /// Print the CPG of this struct
  void debugPrintCloseProximityGraph(raw_ostream& OS) const;
  /// Print the Gold CPG of this struct
  void debugPrintGoldCPT(raw_ostream& OS) const;

 private:
  const Module& CurrentModule;
  const StructFieldAccessManager* StructManager;
  const StructFieldAccessInfo* StructInfo;
  FieldNumType NumElements;
  /// A array of established FRG, used to organize memory
  FRGArrayType FRGArray;
  /// Close Proximity relation table
  CloseProximityTableType CloseProximityTable;
  /// Golden Close Proximity relation table for verifying CPT correctness
  CloseProximityTableType GoldCPT;

 private:
  /// Calculate memory access data size in Bytes
  DataBytesType getMemAccessDataSize(const Instruction* I) const;

  /// Depth-first search the CFG to mark all back edges and record in each BasicBlockHelperInfo
  void markBackEdges(const FieldReferenceGraph* FRG, const BasicBlock* BB, BasicBlockSetType* VisitedBB);

  /// Main function to detect and mark all backedges
  void detectBackEdges(const FieldReferenceGraph* FRG, const Function* F);

  /// Update CPG with a CloseProximity pair
  /// %{
  void updateCPG(FieldNumType Src, FieldNumType Dest, ExecutionCountType C, DataBytesType D);
  void updateGoldCPG(FieldNumType Src, FieldNumType Dest, ExecutionCountType C, DataBytesType D);
  /// %}

  /// Functions used for building CP relations with brutal force, only used for debugging
  /// %{
  /// Calculate CP relations between two nodes with edges along a path
  void calculateCPRelation(EdgeArrayType* Path);

  /// Find all paths between two nodes in CPG
  void findPathInFRG(FieldReferenceGraph::Node* Start, FieldReferenceGraph::Node* End, EdgeArrayType* Path, NodeSetType* VisitedNodes);

  /// Calculate CP relations between two nodes
  void calculatePathBetween(FieldReferenceGraph::Node* FromNode, FieldReferenceGraph::Node* ToNode);

  /// Create pairs of all fields in the structs to calculate CP relations with brutal force
  void createFRGPairs(FieldReferenceGraph* FRG, FieldReferenceGraph::Node* Root, EdgeArrayType* Path);

  /// Main function to create golden CPG with brutal force
  void createGoldCloseProximityRelations(FieldReferenceGraph* FRG);
  /// %}

  /// Functions used for building CP relations with collapsing
  /// %{
  /// Recursively update CPG between node From to the subtree of node To
  void updateCPGBetweenNodes(FieldReferenceGraph::Node* From, FieldReferenceGraph::Node* To, FieldReferenceGraph::Edge* Edge, ExecutionCountType C, DataBytesType D, NodeSetType* CheckList);

  /// Call the recursive function updateCPGBetweenNodes() to update CPG between Edge E starting node to subtree of Edge E
  void updateCPGFromNodeToSubtree(FieldReferenceGraph::Edge* E);

  /// Collapse the subtree of Edge Arc into the edge to store the CP relations between the Edge starting node and all the successors of the edge
  bool collapseSuccessor(FieldReferenceGraph* FRG, FieldReferenceGraph::Edge* Arc);

  /// Recursively collapse the FRG until it becomes the root with a collapse edge
  bool collapseRoot(FieldReferenceGraph* FRG, FieldReferenceGraph::Node* Root);

  /// Main function to create CPG by collapsing FRG
  void collapseFieldReferenceGraph(FieldReferenceGraph* FRG);
  /// %}

  /// Compare CPG results after collapsing with golden CPG
  void compareCloseProximityRelations() const;
};

} // namespace llvm

#endif
