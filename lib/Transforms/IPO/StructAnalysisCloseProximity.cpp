// lib/Tranforms/IPO/StructFieldCacheAnalysis.cpp - Performs Cache-Aware
// Structure Analysis-*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===------------------------------------------------------------------------===//
//
// This files implements FieldReferenceGraph and CloseProximityGraph
//
// FieldReferenceGraph implements Field Reference Graph (FRG) that represents
// relationships between two consecutive field accesses. It looks like Control
// Flow Graph (CFG) except that each node represents a field access and each
// edge between nodes represents how frequent the pair happens consecutively and
// how many bytes of other memory accesses are between the pair on average.
//
// CloseProximityBuilder builds Close Proximity (CP) relations of every pair of
// struct fields of this struct type. The CP relations can be obtained by adding
// up all paths between two nodes in FRG with brutal force, which results in
// exponential complexity. Instead, we use an FRG collapsing algorithm to
// collapse FRG and update CP relations in the same time. The collapsing
// algorithm is trying to summarize the CP relations between a nodes with all
// nodes in its subtree into a table stored with node. With collapsing, a tree
// can be reduced to a node with a table, as long as the CP relations between
// nodes in the table is updated. Thus a FRG will eventually be reduced to a
// root nodes with a relation table, or a much smaller tree with a few
// non-collapsed nodes. In either case, the complexity is much smaller than
// exponential that can complete with reasonable amount of time.
//
//===------------------------------------------------------------------------===//

#include "StructFieldCacheAnalysisImpl.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Analysis/CFG.h"
#include "llvm/Support/Format.h"

#include <cmath>
#include <queue>

using namespace llvm;

#define DEBUG_PRINT_COUNT(x) (format("%.2f", (x)))
#define DEBUG_PRINT_DIST(x) (format("%.3f", (x)))

static cl::opt<bool> PerformFRGOnly(
    "struct-analysis-FRG-only", cl::init(false), cl::Hidden,
    cl::desc("Stop the analysis after performing FRG generation"));

static cl::opt<bool> PerformCPGCheck(
    "struct-analysis-check-CPG", cl::init(false), cl::Hidden,
    cl::desc("Perform CPG checking algorithm that takes a long time"));

static cl::opt<bool> DisableIgnoreZeroCountNodes(
    "struct-analysis-disable-ignore-zeros", cl::init(false), cl::Hidden,
    cl::desc("Ignore zero-count nodes in FRG generation"));

// Utility functions
// Function used to update (count, distance) pair by taking a weighted average
// of the Result and Source and save result to Result. Used when updating
// CPG or updating an edge weight
static void updatePairByMerging(ExecutionCountType &ResultCount,
                                DataBytesType &ResultDistance,
                                ExecutionCountType SourceCount,
                                DataBytesType SourceDistance) {
  if (SourceCount + ResultCount != 0)
    ResultDistance =
        (SourceCount * SourceDistance + ResultCount * ResultDistance) /
        (SourceCount + ResultCount);
  else
    ResultDistance = 0;
  ResultCount += SourceCount;
}

// Function used to update (count, distance) pair. distance is updated by
// directly adding ResultDistance and SourceDistance. count is updated by
// taking minimum of a portion of ResultCount and a portion of SourceCount
// Used when calculating edge weight along a path, especially when nodes
// have multiple in and out edges
static void updatePairByConnecting(ExecutionCountType &ResultCount,
                                   double OutRatio,
                                   DataBytesType &ResultDistance,
                                   ExecutionCountType SourceCount,
                                   double InRatio,
                                   DataBytesType SourceDistance) {
  assert(InRatio <= 1 && OutRatio <= 1);
  ResultCount = std::min(ResultCount * OutRatio, SourceCount * InRatio);
  ResultDistance += SourceDistance;
}

// Functions for FieldReferenceGraph
// Reconnect two nodes with the current Edge when collapsing FRG.
// There are three possible results after this function.
// Case #1. If the reconnection happens from a node to itself, no reconnection
//    happens because we don't care a CP relations between a field with itself.
// Case #2. If the reconnection happends between two nodes that already have
//    a connection, no new edge is added. Instead, remove the existing edge
//    after merging the weights onto the Edge and use Edge to connect the two
//    nodes.
// Case #3. Otherwise, connect the two nodes with current Edge without changing
//    its weights
void FieldReferenceGraph::Edge::reconnect(FieldReferenceGraph::Node *From,
                                          FieldReferenceGraph::Node *To) {
  DEBUG_WITH_TYPE(DEBUG_TYPE_CPG,
                  dbgs() << "Reconnect edge: (" << FromNode->Id << ","
                         << ToNode->Id << ") to (" << From->Id << "," << To->Id
                         << ") with " << DEBUG_PRINT_COUNT(ExecutionCount)
                         << " and " << DEBUG_PRINT_DIST(DataSize) << "\n");
  // Case #1
  if (From == To) {
    DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Reconnection results in a node "
                                              "pointing to itself, remove this "
                                              "edge\n");
    FromNode->OutEdges.erase(this);
    ToNode->InEdges.erase(this);
    FromNode = NULL;
    ToNode = NULL;
    return;
  }
  assert(From != FromNode || To != ToNode); // Assume never reconnect two nodes
                                            // already connected with this Edge
  Edge *ExistEdge = NULL;
  for (auto *E : From->OutEdges) {
    if (E->ToNode == To)
      ExistEdge = E;
  }
  // Case #2
  if (ExistEdge) {
    assert(To->InEdges.find(ExistEdge) != To->InEdges.end());
    DEBUG_WITH_TYPE(DEBUG_TYPE_CPG,
                    dbgs() << "Node " << From->Id << " and Node " << To->Id
                           << " already have an edge with "
                           << DEBUG_PRINT_COUNT(ExistEdge->ExecutionCount)
                           << " and " << DEBUG_PRINT_DIST(ExistEdge->DataSize)
                           << "\n");
    // Reconnect edge to two nodes that have connection, weights need to be
    // adjusted
    updatePairByMerging(ExecutionCount, DataSize, ExistEdge->ExecutionCount,
                        ExistEdge->DataSize);
    From->OutEdges.erase(ExistEdge);
    To->InEdges.erase(ExistEdge);
  }
  // Case #3
  if (From != FromNode)
    FromNode->OutEdges.erase(this);
  if (To != ToNode)
    ToNode->InEdges.erase(this);
  From->OutEdges.insert(this);
  To->InEdges.insert(this);
  FromNode = From;
  ToNode = To;
  DEBUG_WITH_TYPE(DEBUG_TYPE_CPG,
                  dbgs() << "After reconnection: (" << FromNode->Id << ","
                         << ToNode->Id << ") with "
                         << DEBUG_PRINT_COUNT(ExecutionCount) << " and "
                         << DEBUG_PRINT_DIST(DataSize) << "\n");
}

FieldReferenceGraph::BasicBlockHelperInfo *
FieldReferenceGraph::createBasicBlockHelperInfo(const BasicBlock *BB) {
  assert(BBInfoMap.find(BB) == BBInfoMap.end());
  auto *ret = BBInfoMap[BB] = new BasicBlockHelperInfo;
  return ret;
}

FieldReferenceGraph::BasicBlockHelperInfo *
FieldReferenceGraph::getBasicBlockHelperInfo(const BasicBlock *BB) const {
  auto it = BBInfoMap.find(BB);
  assert(it != BBInfoMap.end());
  return it->second;
}

FieldReferenceGraph::Node *
FieldReferenceGraph::createNewNode(FieldNumType FieldNum, DataBytesType S) {
  DEBUG_WITH_TYPE(DEBUG_TYPE_FRG, dbgs() << "Create Node #" << NodeList.size()
                                         << " for field " << FieldNum << "\n");
  auto *Node = new FieldReferenceGraph::Node(NodeList.size(), FieldNum, S);
  // Use NodeList to collect all allocated Nodes. Will be deallocated in
  // destructor to avoid memory leak
  NodeList.push_back(Node);
  return Node;
}

// Connect two nodes by creating an Edge between them and set the weight to
// be (C, D). The only tricky case is when the two nodes are already connected
// by another edge, instead of creating a new edge, we update its weight.
void FieldReferenceGraph::connectNodes(FieldReferenceGraph::Node *From,
                                       FieldReferenceGraph::Node *To,
                                       ExecutionCountType C, DataBytesType D,
                                       bool BackEdge) {
  if (From == To) {
    // Don't connect a node back to itself to avoid a node never be able to
    // collapse
    return;
  }
  // Try to find existing edge between From and To
  Edge *ExistEdge = NULL;
  for (auto *E : From->OutEdges) {
    if (E->ToNode == To)
      ExistEdge = E;
  }
  if (ExistEdge) {
    // If there's already an edge between the two nodes, only increase count and
    // distance but not create a new edge
    DEBUG_WITH_TYPE(
        DEBUG_TYPE_FRG,
        dbgs() << "Connect two nodes that already have a connection\n");
    assert(BackEdge == ExistEdge->LoopArc);
    updatePairByMerging(ExistEdge->ExecutionCount, ExistEdge->DataSize, C, D);
    From->OutSum += C;
    To->InSum += C;
    return;
  }
  auto *Edge = new FieldReferenceGraph::Edge(EdgeList.size(), C, D);
  EdgeList.push_back(Edge);
  Edge->connectNodes(From, To);
  From->OutEdges.insert(Edge);
  To->InEdges.insert(Edge);
  From->OutSum += C;
  To->InSum += C;
  if (BackEdge)
    Edge->LoopArc = true;
}

// Collapse information of a Node to an Edge as a collapsed Entry
void FieldReferenceGraph::collapseNodeToEdge(FieldReferenceGraph::Node *N,
                                             FieldReferenceGraph::Edge *E) {
  assert(E->Collapsed == false);
  E->Collapsed = true;
  if (N->FieldNum == 0)
    return;
  auto *Entry = new FieldReferenceGraph::Entry(EntryList.size(), N->FieldNum,
                                               E->ExecutionCount, E->DataSize);
  E->CollapsedEntries.insert(Entry);
  EntryList.push_back(Entry);
}

// Copy collapsed Entry from one Edge to another
void FieldReferenceGraph::moveCollapsedEntryToEdge(
    FieldReferenceGraph::Entry *Entry, FieldReferenceGraph::Edge *FromEdge,
    FieldReferenceGraph::Edge *ToEdge) {
  assert(FromEdge->Collapsed);
  ToEdge->CollapsedEntries.insert(Entry);
}

// Debug only. Print FRG to a specified output stream in the order of BFS.
// The format of each node in output is like:
// Node 0 accesses 1 and has 2.0 out sum and 2.0 in sum connect with
// { Node 1 (1.0, 0.0), Node 2 (1.0, 4.0) B}.
// Here "B" denotes the edge is a back edge
void FieldReferenceGraph::debugPrint(raw_ostream &OS) const {
  assert(RootNode);
  std::queue<Node *> ExamineList; // List of Nodes to print
  NodeSetType ExaminedSet; // Set of Nodes popped from list to avoid duplicate
  ExamineList.push(RootNode);
  OS << "Field Reference Graph for function: " << Func->getName() << "\n";
  while (!ExamineList.empty()) {
    auto *Node = ExamineList.front();
    ExamineList.pop();
    if (ExaminedSet.find(Node) != ExaminedSet.end())
      continue;
    ExaminedSet.insert(Node);
    OS << "Node " << Node->Id << " accesses " << Node->FieldNum << " and has "
       << DEBUG_PRINT_COUNT(Node->OutSum) << " out sum and "
       << DEBUG_PRINT_COUNT(Node->InSum) << " in sum: ";
    OS << "connect with {";
    for (auto *Edge : Node->OutEdges) {
      OS << " Node " << Edge->ToNode->Id << " ("
         << DEBUG_PRINT_COUNT(Edge->ExecutionCount) << ","
         << DEBUG_PRINT_DIST(Edge->DataSize) << ") "
         << (Edge->LoopArc ? "B" : "") << " ";
      ExamineList.push(Edge->ToNode);
    }
    OS << "}\n";
  }
}

// Debug only. Print collapsed entries on each Edge.
// The output format of each entry is like:
// Collapsed entry for edge: (0, 1)
// Collapsed entry 0 field num: 1, count: 1.0, distance: 4.0
void FieldReferenceGraph::debugPrintCollapsedEntries(
    raw_ostream &OS, FieldReferenceGraph::Edge *E) const {
  OS << "Collapsed entry for edge: (" << E->FromNode->Id << "," << E->ToNode->Id
     << ")\n";
  for (auto *CE : E->CollapsedEntries) {
    OS << "Collapsed entry " << CE->Id << " field num: " << CE->FieldNum
       << ", count: " << DEBUG_PRINT_COUNT(CE->ExecutionCount)
       << ", distance: " << DEBUG_PRINT_DIST(CE->DataSize) << "\n";
  }
}

// Functions for CloseProximityBuilder
// Constructor of CloseProximityBuilder. Mainly used to initialize
// two CP tables: CloseProximityTable and GoldCPT
CloseProximityBuilder::CloseProximityBuilder(const Module &M,
                                             const StructFieldAccessManager *SM,
                                             const StructFieldAccessInfo *SI)
    : CurrentModule(M), StructManager(SM), StructInfo(SI) {
  assert(StructInfo);
  NumElements = StructInfo->getNumElements();
  FRGArray.clear();
  CloseProximityTable.resize(NumElements);
  for (unsigned i = 0; i < NumElements; i++) {
    CloseProximityTable[i].resize(NumElements);
    for (unsigned j = 0; j < NumElements; j++) {
      CloseProximityTable[i][j] = std::make_pair(0, 0);
    }
  }
  if (PerformCPGCheck) {
    GoldCPT.resize(NumElements);
    for (unsigned i = 0; i < NumElements; i++) {
      GoldCPT[i].resize(NumElements);
      for (unsigned j = 0; j < NumElements; j++) {
        GoldCPT[i][j] = std::make_pair(0, 0);
      }
    }
  }
}

// Get the size of the data accessed by this load/store instruction.
// Return number of bytes accessed by this instruction
DataBytesType
CloseProximityBuilder::getMemAccessDataSize(const Instruction *I) const {
  assert(I->getOpcode() == Instruction::Load ||
         I->getOpcode() == Instruction::Store);
  Type *type;
  if (I->getOpcode() == Instruction::Load)
    type = I->getType();
  else
    type = I->getOperand(0)->getType();
  assert(type->isSized());
  return CurrentModule.getDataLayout().getTypeSizeInBits(type) / 8;
}

// A wrapper function to find all backedges on CFG. It wraps a call to
// function FindFunctionBackedges() in CFG.h to find all backeges and record
// backeges in the each BasicBlockHelperInfo of the BB
void CloseProximityBuilder::detectBackEdges(const FieldReferenceGraph *FRG,
                                            const Function *F) {
  SmallVector<std::pair<const BasicBlock *, const BasicBlock *>, 32> BackEdges;
  FindFunctionBackedges(*F, BackEdges);
  for (auto it = BackEdges.begin(); it != BackEdges.end(); it++) {
    auto *BBI = FRG->getBasicBlockHelperInfo(it->first);
    BBI->BackEdgeSet.insert(it->second);
  }
}

// Main function to build Field Reference Graph (FRG) and returns its pointer
// The function consists of three parts.
// 1. Build sub-FRG for each basic block. In this part, find all consecutive
//    load/store field accesses in the basic block and connect each other with
//    the execution count of the basic block and the distance in number of bytes
//    of normal load/store instructions between them.
//    There are two special cases: 1) if there's no memory accesses in the
//    basic block, create a dummy node; 2) if the first load/store is not on
//    any field, create a dummy node.
// 2. Find all backedges and record them in each BasicBlockHelperInfo
// 3. Build FRG by connecting all sub-FRGs. In this part, the connection is
//    based on the branches connecting BBs in CFG. The count is taken by the
//    minimum of branch probability and the execution count of following BB/
//    The distance is the remaining bytes in each previous BB, i.e. number of
//    bytes accessed after the last node.
FieldReferenceGraph *
CloseProximityBuilder::buildFieldReferenceGraph(const Function *F) {
  DEBUG_WITH_TYPE(DEBUG_TYPE_FRG, dbgs() << "Create a new empty FRG\n");
  auto *FRG = new FieldReferenceGraph(F);
  FRGArray.push_back(FRG);
  // Create and connect node inside each basic block
  for (auto &BB : *F) {
    DEBUG_WITH_TYPE(DEBUG_TYPE_FRG,
                    dbgs() << "Build partial FRG for BB: " << BB << "\n");
    auto *BBI = FRG->createBasicBlockHelperInfo(&BB);
    for (auto &I : BB) {
      if (auto FieldNum = StructInfo->getAccessFieldNum(&I)) {
        // Case that I is a struct access
        DEBUG_WITH_TYPE(DEBUG_TYPE_FRG, dbgs()
                                            << "Found an instruction " << I
                                            << " is a struct access on field ["
                                            << FieldNum.getValue() << "]\n");
        auto *NewNode =
            FRG->createNewNode(FieldNum.getValue(), getMemAccessDataSize(&I));
        if (BBI->LastNode) {
          // If there's already a node created in the BB, connect the current
          // node with previous one
          DEBUG_WITH_TYPE(DEBUG_TYPE_FRG,
                          dbgs() << "Previous nodes found in the BB\n");
          auto C = 0;
          if (auto ExCnt = StructInfo->getExecutionCount(&I))
            C = ExCnt.getValue();
          auto D = BBI->RemainBytes;
          FRG->connectNodes(BBI->LastNode, NewNode, C, D);
          BBI->RemainBytes = 0;
          DEBUG_WITH_TYPE(
              DEBUG_TYPE_FRG,
              dbgs() << "Connect new node with previous node in the BB: Field ["
                     << BBI->LastNode->FieldNum << "] to ["
                     << FieldNum.getValue() << "] with count: " << C
                     << " and data " << D << " bytes \n");
          BBI->LastNode = NewNode;
        } else {
          // If there's no previous node, update BBI info
          DEBUG_WITH_TYPE(
              DEBUG_TYPE_FRG,
              dbgs() << "No previous node found. It is the first node\n");
          BBI->FirstNode = BBI->LastNode = NewNode;
        }
      } else {
        // Case that I is not struct access but a memory access
        if (I.getOpcode() == Instruction::Load ||
            I.getOpcode() == Instruction::Store) {
          DEBUG_WITH_TYPE(DEBUG_TYPE_FRG,
                          dbgs()
                              << "Found an instruction " << I
                              << " is not a struct access but a load/store.\n");
          if (BBI->LastNode == NULL) {
            // Create a dummy node for the first non-struct memory access
            DEBUG_WITH_TYPE(
                DEBUG_TYPE_FRG,
                dbgs() << "Create a dummy node as the first node in the BB.\n");
            BBI->FirstNode = BBI->LastNode = FRG->createNewNode();
          } else {
            // Record the bytes accessed, used to calculate distance
            BBI->RemainBytes += getMemAccessDataSize(&I);
            DEBUG_WITH_TYPE(DEBUG_TYPE_FRG,
                            dbgs() << "Increment remaining data size to "
                                   << BBI->RemainBytes << "\n");
          }
        }
      }
    }
    if (BBI->LastNode == NULL) {
      // If all instructions are examined and the BB doesn't have any memory
      // accesses, create a dummy node
      assert(BBI->FirstNode == NULL);
      DEBUG_WITH_TYPE(
          DEBUG_TYPE_FRG,
          dbgs()
              << "Create a dummy node as BB does not have memory accesses\n");
      BBI->FirstNode = BBI->LastNode = FRG->createNewNode();
    }
  }
  // Before connect nodes, need to mark all back edges first
  detectBackEdges(FRG, F);
  // Connect nodes between different basic blocks
  for (auto &BB : *F) {
    auto *BBI = FRG->getBasicBlockHelperInfo(&BB);
    auto *Term = BB.getTerminator();
    // Check all the connections from BB in CFG
    for (const auto *SB : Term->successors()) {
      auto *SBI = FRG->getBasicBlockHelperInfo(SB);
      ExecutionCountType C = 0;
      auto BBCount = StructInfo->getExecutionCount(&BB);
      auto SBCount = StructInfo->getExecutionCount(SB);
      if (BBCount.hasValue() && SBCount.hasValue()) {
        // Take probability of the branch
        auto Prob = StructManager->getBranchProbability(&BB, SB);
        assert(Prob.hasValue());
        C = std::min(BBCount.getValue() * Prob.getValue(),
                     SBCount.getValue() * 1.0);
      }
      if (!DisableIgnoreZeroCountNodes && C < 1e-3)
        continue;
      auto D = BBI->RemainBytes; // Use size of remaining data in BB
      auto isBackEdge = (BBI->BackEdgeSet.find(SB) != BBI->BackEdgeSet.end());
      FRG->connectNodes(BBI->LastNode, SBI->FirstNode, C, D, isBackEdge);
    }
  }
  assert(StructInfo->getExecutionCount(&F->getEntryBlock()));
  auto *BBI = FRG->getBasicBlockHelperInfo(&F->getEntryBlock());
  FRG->setRootNode(BBI->FirstNode);
  DEBUG_WITH_TYPE(DEBUG_TYPE_FRG, dbgs()
                                      << "---------- FRG for function "
                                      << F->getName() << "----------------\n");
  DEBUG_WITH_TYPE(DEBUG_TYPE_FRG, FRG->debugPrint(dbgs()));
  DEBUG_WITH_TYPE(DEBUG_TYPE_FRG, dbgs() << "----------------------------------"
                                            "----------------------------------"
                                            "----\n");
  return FRG;
}

// Core function to update a cell in CPT.
// In this function, a CPT can be either CloseProximityTable (real CP relations)
// or GoldCPT (used for result validation). A cell in CPT represents CP
// relations between two fields and consists of a pair of numbers: count and
// distance. When updating a cell, call updatePairByMerging().
void CloseProximityBuilder::updateCPT(FieldNumType Src, FieldNumType Dest,
                                      ExecutionCountType C, DataBytesType D,
                                      CloseProximityTableType &CPT) {
  assert(Src <= NumElements && Dest <= NumElements);
  if (Src == 0 || Dest == 0 || C < 1e-3 || Src == Dest)
    // Give up update if from or to a dummy node, or if the count is zero
    return;
  assert(!std::isnan(C));
  if (Src != 0 && Dest != 0) {
    if (Src > Dest) {
      std::swap(Src, Dest);
    }

    DEBUG_WITH_TYPE(
        DEBUG_TYPE_CPG,
        dbgs() << " Before update: "
               << "(" << DEBUG_PRINT_COUNT(CPT[Src - 1][Dest - 1].first) << ","
               << DEBUG_PRINT_DIST(CPT[Src - 1][Dest - 1].second) << ")\n");
    assert(CPT[Src - 1][Dest - 1].first + C != 0);
    updatePairByMerging(CPT[Src - 1][Dest - 1].first,
                        CPT[Src - 1][Dest - 1].second, C, D);
    if (std::isnan(CPT[Src - 1][Dest - 1].first) ||
        std::isnan(CPT[Src - 1][Dest - 1].second)) {
      errs() << "Update CPG between " << Src << " and " << Dest
             << " with count " << DEBUG_PRINT_COUNT(C) << " and distance "
             << DEBUG_PRINT_DIST(D) << "\n";
    }
    DEBUG_WITH_TYPE(
        DEBUG_TYPE_CPG,
        dbgs() << " After update: "
               << "(" << DEBUG_PRINT_COUNT(CPT[Src - 1][Dest - 1].first) << ","
               << DEBUG_PRINT_DIST(CPT[Src - 1][Dest - 1].second) << ")\n");
  }
}

// Wrapper function to update a cell in CloseProximityTable
void CloseProximityBuilder::updateCPG(FieldNumType Src, FieldNumType Dest,
                                      ExecutionCountType C, DataBytesType D) {
  DEBUG_WITH_TYPE(DEBUG_TYPE_CPG,
                  dbgs() << "Will update CPG between " << Src << " and " << Dest
                         << " with count " << DEBUG_PRINT_COUNT(C)
                         << " and distance " << DEBUG_PRINT_DIST(D) << ":\n");
  updateCPT(Src, Dest, C, D, CloseProximityTable);
}

// Wrapper function to update a cell in GoldCPT
void CloseProximityBuilder::updateGoldCPG(FieldNumType Src, FieldNumType Dest,
                                          ExecutionCountType C,
                                          DataBytesType D) {
  DEBUG_WITH_TYPE(DEBUG_TYPE_CPG,
                  dbgs() << "Will update Gold CPG between " << Src << " and "
                         << Dest << " with count " << DEBUG_PRINT_COUNT(C)
                         << " and distance " << DEBUG_PRINT_DIST(D) << ":\n");
  updateCPT(Src, Dest, C, D, GoldCPT);
}

// Core code to update CP relations along a path from Node From in its subtree
// Node To is a node on its subtree and the path ends at either a leaf node or
// a back edge to a node on the path. Every pair of (From, To) on the path will
// be updated. Edge Arc is used to keep record of the current Edge on the path
// to be calculated.
// There are possible two ways to calculate:
// 1. If the next Edge on the path is a collapsed Edge, the path ends here and
//    update CP relations between Node From and all collapsed entries on the
//    Edge. When updating, make sure to take only a portion of the count on the
//    collapsed entry when the collapsed Edge is one of the incoming edge of the
//    node.
// 2. If the next Edge is not collapsed, update CP relation between From and To
//    and move on to the destination node of the Edge. When moving on,
//    considering the current node might have multiple exits, take a portion of
//    the weight is important to ensure correctness. Call
//    updatePairByConnecting() to only take a portion count when recursively
//    calling the function.
// The following example shows overall guidance to guarantee correctness
// when updating counts: When updating CP relations between A and D,
//
//       Count1      Count5     Count8
//    A --------- B -------- C -------- D
// Suppose A has two outgoing edges: Count1 and Count2, B has two incoming
// edges: Count1 and Count3 and two outgoing edges: Count4 and Count5. C has two
// incoming edges: Count5 and Count6 and two outgoing edges: Count7 and Count8.
// D has two incoming edges: Count9 and Count8. In this case, when we update A
// and D from the order of A->B->C->D, the result of count should be: (Order 1)
//                    Count5                    Count8
// min { Count1 * -------------- , Count5 * --------------- , Count8 }
//                Count4+Count5              Count7+Count8
//
// Meanwhile, when we updating A and from the order of D->C->B->A, we need to
// make sure the results are the same, so the result should be: (Order 2)
//                            Count5                    Count1
// min { Count8, Count8 * -------------- , Count5 * --------------- }
//                         Count5+Count6              Count1+Count3
//
// The results are the same if Node B and C has the same incoming and outgoing
// counts: Count4+Count5=Count1+Count3 and Count5+Count6=Count7+Count8.
//
// When updating CP relation along a path, it's Order 1. When collapsing a node
// to its predecessor edge, it's Order 2. Thus this function is a combination of
// Order 1 and 2 so we need to make sure the counts are correct when connecting
// them.
void CloseProximityBuilder::updateCPGBetweenNodes(
    FieldReferenceGraph::Node *From, FieldReferenceGraph::Node *To,
    FieldReferenceGraph::Edge *Arc, ExecutionCountType C, DataBytesType D,
    NodeSetType *CheckList) {
  if (To == NULL || To == From || CheckList->find(To) != CheckList->end())
    return;
  DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Update CPG from " << From->Id
                                         << " to " << To->Id << " with "
                                         << DEBUG_PRINT_COUNT(C) << " and "
                                         << DEBUG_PRINT_DIST(D) << "\n");
  assert(!std::isnan(C));
  updateCPG(From->FieldNum, To->FieldNum, C, D);
  CheckList->insert(To);
  for (auto *E : To->OutEdges) {
    if (E->ToNode == From)
      continue;
    if (E->Collapsed) {
      DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Update CPG from " << From->Id
                                             << " to collapsed subtree edge ("
                                             << E->FromNode->Id << ","
                                             << E->ToNode->Id << ") with ratio "
                                             << E->ExecutionCount / To->OutSum
                                             << "\n");
      for (auto *Entry : E->CollapsedEntries) {
        auto ExCnt = C;
        auto Dist = D;
        assert(To->InSum > 0);
        updatePairByConnecting(
            ExCnt, E->ExecutionCount / To->OutSum, Dist, Entry->ExecutionCount,
            Arc->ExecutionCount / To->InSum, To->Size + Entry->DataSize);
        assert(!std::isnan(ExCnt));
        updateCPG(From->FieldNum, Entry->FieldNum, ExCnt, Dist);
      }
    } else {
      DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Found non-collapsed successor "
                                             << E->ToNode->Id << "\n");
      auto ExCnt = C;
      auto Dist = D;
      assert(To->OutSum > 0);
      updatePairByConnecting(ExCnt, E->ExecutionCount / To->OutSum, Dist, ExCnt,
                             1.0, To->Size + E->DataSize);
      if (ExCnt > 1e-3)
        updateCPGBetweenNodes(From, E->ToNode, E, ExCnt, Dist, CheckList);
    }
  }
  CheckList->erase(To);
}

// Main function to update a node to its subtree. It's a wrapper function to
// call the updateCPGBetweenNodes() function recursively. The function is used
// when disconnecting an edge in collapsing FRG
void CloseProximityBuilder::updateCPGFromNodeToSubtree(
    FieldReferenceGraph::Edge *E) {
  if (E->FromNode->FieldNum == 0 || E->ExecutionCount <= 1e-3)
    return;
  DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Update CPG from "
                                         << E->FromNode->Id << " to subtree "
                                         << E->ToNode->Id << "\n");
  NodeSetType CheckList;
  updateCPGBetweenNodes(E->FromNode, E->ToNode, E, E->ExecutionCount,
                        E->DataSize, &CheckList);
}

// Core function to collapse FRG. Edge Arc is the edge that is going to be
// collapsed, i.e. only has one predecessor. Collapse this Edge means to add
// the direct successor of this Edge (Arc->ToNode) to the collapesd entries
// of this edge. If the node has outgoing edges, which should be a common case,
// there are two possible solutions depending on if the edge is collapsed:
// 1. If the outgoing edge is collapsed, i.e. it connects to a transformed leaf
//    node, we only need to update CP relations between the source Node and all
//    the collapsed entries and also move those collapsed entries to the Edge
//    Arc. Before update CP relations or move collapsed entries, we need to
//    recalculate the count in the entry according to the Order 2 above (see
//    comments before updateCPGBetweenNodes() ).
// 2. If the outgoing edge is not collapsed, we need to move the subtree to the
//    source Node (Arc->FromNode), because the node Arc->ToNode is going to be
//    collapsed and "disappeared". This disconnection needs to call
//    updateCPGFromNodeToSubtree() to make sure we don't lose any information
//    and the reconnection needs to call Edge::reconnect() to update weights.
bool CloseProximityBuilder::collapseSuccessor(FieldReferenceGraph *FRG,
                                              FieldReferenceGraph::Edge *Arc) {
  bool Ret = false;
  DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Collapse successor Edge ("
                                         << Arc->FromNode->Id << ","
                                         << Arc->ToNode->Id << ")\n");
  assert(!std::isnan(Arc->ExecutionCount));
  updateCPG(Arc->FromNode->FieldNum, Arc->ToNode->FieldNum, Arc->ExecutionCount,
            Arc->DataSize);
  FRG->collapseNodeToEdge(Arc->ToNode, Arc);
  auto Edges = Arc->ToNode->OutEdges;
  assert(Arc->ToNode->InSum > 0);
  auto Ratio = Arc->ExecutionCount / Arc->ToNode->InSum;
  for (auto *E : Edges) {
    assert(E->FromNode == Arc->ToNode);
    DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Check edge: (" << E->FromNode->Id
                                           << "," << E->ToNode->Id << ")\n");
    if (E->Collapsed) {
      for (auto *Entry : E->CollapsedEntries) {
        DEBUG_WITH_TYPE(DEBUG_TYPE_CPG,
                        dbgs() << "Move collapsed entry " << Entry->Id
                               << " from edge: (" << E->FromNode->Id << ","
                               << E->ToNode->Id << ") to (" << Arc->FromNode->Id
                               << "," << Arc->ToNode->Id << ") with a ratio of "
                               << format("%.3f", Ratio) << "\n");
        Entry->ExecutionCount =
            std::min(Entry->ExecutionCount * Ratio, Arc->ExecutionCount);
        Entry->DataSize += Arc->DataSize + Arc->ToNode->Size;
        assert(!std::isnan(Entry->ExecutionCount));
        updateCPG(Arc->FromNode->FieldNum, Entry->FieldNum,
                  Entry->ExecutionCount, Entry->DataSize);
        FRG->moveCollapsedEntryToEdge(Entry, E, Arc);
      }
    } else {
      updateCPGFromNodeToSubtree(E);
      E->ExecutionCount =
          std::min(Arc->ExecutionCount, E->ExecutionCount * Ratio);
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

// Main function to collapse FRG and update CloseProximityTable.
// The function provides an order of examinations on the FRG: we make sure
// all the successors are examined before a node is examined (first for-loop
// in the while-loop) and if any of the successors are restructured, instead
// of going up, we re-examine the successors, which is also guarantee that
// when trying to collapsing a node, all of its successors are either collapsed
// or proved to be not collapsable.
bool CloseProximityBuilder::collapseRoot(FieldReferenceGraph *FRG,
                                         FieldReferenceGraph::Node *Root) {
  DEBUG_WITH_TYPE(DEBUG_TYPE_CPG,
                  dbgs() << "Collapse Node " << Root->Id << " as root\n");
  bool change = true;
  bool SubtreeChange = false;
  while (change) {
    change = false;
    for (auto *E : Root->OutEdges) {
      if (!E->LoopArc)
        change |= collapseRoot(FRG, E->ToNode);
    }
    for (auto *E : Root->OutEdges) {
      DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Check out edge: ("
                                             << E->FromNode->Id << ","
                                             << E->ToNode->Id << "): ");
      if (E->LoopArc) {
        DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "It is a loop arc\n");
      } else if (E->ToNode->InEdges.size() == 1 && !E->Collapsed) {
        // if SUCC can be collapsed, i.e. only has one predecessor
        DEBUG_WITH_TYPE(DEBUG_TYPE_CPG,
                        dbgs() << "Found a successor can be collapsed\n");
        if (collapseSuccessor(FRG, E)) {
          SubtreeChange = true;
          change = true;
        }
      } else {
        DEBUG_WITH_TYPE(
            DEBUG_TYPE_CPG,
            dbgs() << "Successor cannot collapse and not a loop arc\n");
      }
    }
    if (change)
      DEBUG_WITH_TYPE(DEBUG_TYPE_CPG,
                      dbgs() << "Subtree changes, need to collapse again\n");
  }
  return SubtreeChange;
}

// Function to create CP relations on a collapsed FRG. If an FRG is not not
// collapsed to a single Root node, we need to use this function to find all
// the non-collapsed nodes from bottom to up (first for-loop) and use brutal
// force to build CP relations between the node and its subtree (second
// for-loop).
void CloseProximityBuilder::createCloseProximityRelations(
    FieldReferenceGraph *FRG, FieldReferenceGraph::Node *Root) {
  DEBUG_WITH_TYPE(DEBUG_TYPE_CPG,
                  dbgs() << "Finalize Node " << Root->Id << " as root\n");
  // Traverse the remaining FRG and calculate CP relations from bottom of the
  // FRG
  for (auto *E : Root->OutEdges) {
    if (!E->LoopArc && !E->Collapsed && !E->ToNode->Visited)
      createCloseProximityRelations(FRG, E->ToNode);
  }
  // Calculate CP relations between each node and its non-collapsed successors
  DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Calculate CP-relations with Node "
                                         << Root->Id << " as root\n");
  for (auto *E : Root->OutEdges) {
    if (!E->Collapsed) {
      DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Found a non-collapsed edge: ("
                                             << E->FromNode->Id << ","
                                             << E->ToNode->Id << ")\n");
      updateCPGFromNodeToSubtree(E);
    }
  }
  Root->Visited = true;
}

// Main function to collpase FRG and create CP relations. First call
// collapseRoot() to collapse FRG and if there's an outgoing edge of Root is
// not collapsed, call createCloseProximityRelations() to build CP relations
// with brutal force.
void CloseProximityBuilder::collapseFieldReferenceGraph(
    FieldReferenceGraph *FRG) {
  auto *Root = FRG->getRootNode();
  collapseRoot(FRG, Root);
  // If the FRG is not collapsed to a single node, need calculate CP relations
  // of remaining nodes with brutal force
  auto AllCollapsed = true;
  for (auto *E : Root->OutEdges) {
    if (!E->Collapsed)
      AllCollapsed = false;
  }
  // FIXME: Use this assertion to detect if this happens.
  if (AllCollapsed)
    return;
  createCloseProximityRelations(FRG, Root);
}

// Functions for building CPG with brutal force, only used for debugging
// (Debug Only) Function to calculate CP relations between the first node
// and the last node along a path and update to GoldCPT for debugging
void CloseProximityBuilder::calculateCPRelation(EdgeArrayType *Path) {
  DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Calculate path: \n");
  for (auto *E : *Path) {
    DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "(" << E->FromNode->Id << ","
                                           << E->ToNode->Id << ")"
                                           << ",");
  }
  DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "\n");
  assert(Path->size());
  auto C = (*Path)[0]->ExecutionCount;
  auto D = (*Path)[0]->DataSize;
  for (unsigned i = 1; i < Path->size(); i++) {
    auto *E = (*Path)[i];
    updatePairByConnecting(C, E->ExecutionCount / E->FromNode->OutSum, D,
                           E->ExecutionCount, 1.0,
                           E->DataSize + E->FromNode->Size);
  }
  updateGoldCPG((*Path)[0]->FromNode->FieldNum,
                (*Path)[Path->size() - 1]->ToNode->FieldNum, C, D);
}

// (Debug only) Function to find a path in FRG. The function has to make
// sure the path ends on either leaf node or a back edge to a visited node
// on the path. After a path is found, call calculateCPRelation() to update
// golden CP relations. The function is recursively called to find all paths
// between two nodes.
void CloseProximityBuilder::findPathInFRG(FieldReferenceGraph::Node *Start,
                                          FieldReferenceGraph::Node *End,
                                          EdgeArrayType *Path,
                                          NodeSetType *VisitedNodes) {
  for (auto *E : Start->OutEdges) {
    if (E->ToNode == End) {
      Path->push_back(E);
      DEBUG_WITH_TYPE(DEBUG_TYPE_CPG,
                      dbgs() << "Find the destination, gonna calculate\n");
      calculateCPRelation(Path);
      Path->pop_back();
    } else if (VisitedNodes->find(E->ToNode) != VisitedNodes->end()) {
      DEBUG_WITH_TYPE(DEBUG_TYPE_CPG,
                      dbgs() << "Loop found, don\'t continue to find path\n");
      continue;
    } else if (E->ExecutionCount) {
      Path->push_back(E);
      VisitedNodes->insert(E->ToNode);
      findPathInFRG(E->ToNode, End, Path, VisitedNodes);
      Path->pop_back();
      VisitedNodes->erase(E->ToNode);
    }
  }
}

// (Debug only) Wrapper function to find all paths between FromNode to ToNode.
void CloseProximityBuilder::calculatePathBetween(
    FieldReferenceGraph::Node *FromNode, FieldReferenceGraph::Node *ToNode) {
  EdgeArrayType Path;
  NodeSetType VisitedNodes;
  DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Calculate Path between "
                                         << FromNode->Id << " and "
                                         << ToNode->Id << "\n");
  assert(FromNode != ToNode);
  VisitedNodes.insert(FromNode);
  findPathInFRG(FromNode, ToNode, &Path, &VisitedNodes);
}

// (Debug only) Function to create a pair between every node in FRG. In order
// to save time on some obvious cases, we don't calculate CP relations for
// dummy nodes, a node to itself or two nodes have the same field number.
void CloseProximityBuilder::createFRGPairs(FieldReferenceGraph *FRG,
                                           FieldReferenceGraph::Node *Root,
                                           EdgeArrayType *Path) {
  for (unsigned i = 0; i < FRG->getNumNodes(); i++) {
    auto *FromNode = FRG->getNodeById(i);
    if (FromNode->FieldNum == 0) // Skip dummy nodes
      continue;
    for (unsigned j = 0; j < FRG->getNumNodes(); j++) {
      if (i == j)
        continue;
      auto *ToNode = FRG->getNodeById(j);
      if (ToNode->FieldNum == 0) // Skip dummy nodes
        continue;
      if (FromNode->FieldNum == ToNode->FieldNum)
        continue;
      calculatePathBetween(FromNode, ToNode);
    }
  }
}

// (Debug only) Main function to create golden CP relations.
void CloseProximityBuilder::createGoldCloseProximityRelations(
    FieldReferenceGraph *FRG) {
  auto *Root = FRG->getRootNode();
  EdgeArrayType Path;
  DEBUG_WITH_TYPE(DEBUG_TYPE_CPG,
                  dbgs() << "Create CP Relations for FRG with brutal force\n");
  createFRGPairs(FRG, Root, &Path);
}

// (Debug only) Function to compare results of CloseProximityTable and
// GoldCPT. The error tolerance is 10% on count and distance.
void CloseProximityBuilder::compareCloseProximityRelations() const {
  for (unsigned i = 0; i < NumElements; i++) {
    for (unsigned j = i + 1; j < NumElements; j++) {
      if (std::abs(GoldCPT[i][j].first - CloseProximityTable[i][j].first) /
                  GoldCPT[i][j].first >
              0.1 ||
          std::abs(GoldCPT[i][j].second - CloseProximityTable[i][j].second) /
                  GoldCPT[i][j].second >
              0.1) {
        outs() << "Found error in CPG: F" << i + 1 << " and F" << j + 1
               << " should be (" << DEBUG_PRINT_COUNT(GoldCPT[i][j].first)
               << "," << DEBUG_PRINT_DIST(GoldCPT[i][j].second)
               << ") but calculated as ("
               << DEBUG_PRINT_COUNT(CloseProximityTable[i][j].first) << ","
               << DEBUG_PRINT_DIST(CloseProximityTable[i][j].second) << ")\n";
      }
    }
  }
}

// Main function to build CP relations. Called by StructFieldAccessManager
// to build CP relations on the structure
void CloseProximityBuilder::buildCloseProximityRelations() {
  for (auto &F : CurrentModule) {
    if (F.isDeclaration())
      continue;
    if (!F.getEntryCount() || F.getEntryCount().getValue() == 0) {
      DEBUG_WITH_TYPE(DEBUG_TYPE_FRG,
                      dbgs() << "Function " << F
                             << " is never executed in profiling\n");
      continue;
    }
    if (!StructInfo->isFunctionToAnalyze(&F)) {
      DEBUG_WITH_TYPE(DEBUG_TYPE_FRG,
                      dbgs() << "Function " << F
                             << " is not in function to analyze\n");
      continue;
    }
    DEBUG_WITH_TYPE(DEBUG_TYPE_CPG,
                    dbgs() << "Analyzing function " << F.getName() << "\n");
    auto *FRG = buildFieldReferenceGraph(&F);
    assert(FRG);
    if (PerformFRGOnly)
      continue;
    // Brutal force get CP relations, should perform first because it preserves
    // FRG
    if (PerformCPGCheck)
      createGoldCloseProximityRelations(FRG);
    // Collpase FRG and get CPG
    collapseFieldReferenceGraph(FRG);
  }
  if (PerformCPGCheck)
    compareCloseProximityRelations();
}

// (Debug only) Debug print FRG
void CloseProximityBuilder::debugPrintFieldReferenceGraph(
    raw_ostream &OS) const {
  for (auto *FRG : FRGArray) {
    FRG->debugPrint(OS);
  }
}

// (Debug only) Main debug print function. If the pass only performs FRG
// generation, print FRG. Otherwise, print CloseProximityTable and if
// golden checker enable, also print GoldCPT.
void CloseProximityBuilder::debugPrintCloseProximityGraph(
    raw_ostream &OS) const {
  if (PerformFRGOnly) {
    debugPrintFieldReferenceGraph(OS);
    return;
  }
  for (unsigned i = 0; i < NumElements; i++) {
    OS << "F" << i + 1 << ": ";
    for (unsigned j = 0; j < NumElements; j++) {
      if (j <= i)
        OS << "-\t";
      else
        OS << "(" << DEBUG_PRINT_COUNT(CloseProximityTable[i][j].first) << ","
           << DEBUG_PRINT_DIST(CloseProximityTable[i][j].second) << ")\t";
    }
    OS << "\n";
  }
  if (PerformCPGCheck)
    debugPrintGoldCPT(OS);
}

// (Debug only) Debug print GoldCPT
void CloseProximityBuilder::debugPrintGoldCPT(raw_ostream &OS) const {
  for (unsigned i = 0; i < NumElements; i++) {
    OS << "F" << i + 1 << ": ";
    for (unsigned j = 0; j < NumElements; j++) {
      if (j <= i)
        OS << "-\t";
      else
        OS << "(" << DEBUG_PRINT_COUNT(GoldCPT[i][j].first) << ","
           << DEBUG_PRINT_DIST(GoldCPT[i][j].second) << ")\t";
    }
    OS << "\n";
  }
}
