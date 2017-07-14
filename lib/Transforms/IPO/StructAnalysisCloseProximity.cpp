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
// This pass performs analysis on cache-aware structure field accesses based on
// the following paper and reports recommendations on changes to make on the
// source code to improve performance.
//  [1] M. Hagog, C. Tice “Cache Aware Data Layout Reorganization Optimization
//  in GCC”, Proceedings
//      of the GCC Developers’ Summit,  Ottawa, 2005.
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
static void updatePairByMerging(ExecutionCountType& ResultCount, DataBytesType& ResultDistance, ExecutionCountType SourceCount, DataBytesType SourceDistance)
{
  if (SourceCount + ResultCount != 0)
    ResultDistance = (SourceCount * SourceDistance + ResultCount * ResultDistance) / (SourceCount + ResultCount);
  else
    ResultDistance = 0;
  ResultCount += SourceCount;
}

static void updatePairByConnecting(ExecutionCountType& ResultCount, double OutRatio, DataBytesType& ResultDistance, ExecutionCountType SourceCount, double InRatio, DataBytesType SourceDistance)
{
  assert(InRatio <= 1 && OutRatio <= 1);
  ResultCount = std::min(ResultCount*OutRatio, SourceCount * InRatio);
  ResultDistance += SourceDistance;
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

FieldReferenceGraph::BasicBlockHelperInfo* FieldReferenceGraph::getBasicBlockHelperInfo(const BasicBlock* BB) const
{
  auto it = BBInfoMap.find(BB);
  assert (it != BBInfoMap.end());
  return it->second;
}

FieldReferenceGraph::Node* FieldReferenceGraph::createNewNode(FieldNumType FieldNum, DataBytesType S)
{
  DEBUG_WITH_TYPE(DEBUG_TYPE_FRG, dbgs() << "Create Node #" << NodeList.size() << " for field " << FieldNum << "\n");
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
  if (N->FieldNum == 0)
    return;
  auto* Entry = new FieldReferenceGraph::Entry(EntryList.size(), N->FieldNum, E->ExecutionCount, E->DataSize);
  E->CollapsedEntries.insert(Entry);
  EntryList.push_back(Entry);
}

void FieldReferenceGraph::moveCollapsedEntryToEdge(FieldReferenceGraph::Entry* Entry, FieldReferenceGraph::Edge* FromEdge, FieldReferenceGraph::Edge* ToEdge)
{
  assert(FromEdge->Collapsed);
  ToEdge->CollapsedEntries.insert(Entry);
}

void FieldReferenceGraph::debugPrint(raw_ostream& OS) const
{
  assert(RootNode);
  std::queue<Node*> ExamineList; // List of Nodes to print
  NodeSetType ExaminedSet; // Set of Nodes popped from list to avoid duplicate
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
      OS << " Node " << Edge->ToNode->Id << " (" << DEBUG_PRINT_COUNT(Edge->ExecutionCount) << "," << DEBUG_PRINT_DIST(Edge->DataSize) << ") " << (Edge->LoopArc ? "B" : "") << " ";
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

// Functions for CloseProximityBuilder
CloseProximityBuilder::CloseProximityBuilder(const Module& M, const StructFieldAccessManager* SM, const StructFieldAccessInfo* SI):
    CurrentModule(M), StructManager(SM), StructInfo(SI) {
  assert(StructInfo);
  NumElements = StructInfo->getNumElements();
  FRGArray.clear();
  CloseProximityTable.resize(NumElements);
  for (unsigned i = 0; i < NumElements; i++){
    CloseProximityTable[i].resize(NumElements);
    for (unsigned j = 0; j < NumElements; j++){
      CloseProximityTable[i][j] = std::make_pair(0, 0);
    }
  }
  if (PerformCPGCheck){
    GoldCPT.resize(NumElements);
    for (unsigned i = 0; i < NumElements; i++){
      GoldCPT[i].resize(NumElements);
      for (unsigned j = 0; j < NumElements; j++){
        GoldCPT[i][j] = std::make_pair(0, 0);
      }
    }
  }
}

DataBytesType CloseProximityBuilder::getMemAccessDataSize(const Instruction* I) const
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

void CloseProximityBuilder::detectBackEdges(const FieldReferenceGraph* FRG, const Function* F)
{
  SmallVector<std::pair<const BasicBlock *, const BasicBlock *>, 32> BackEdges;
  FindFunctionBackedges(*F, BackEdges);
  for (auto it = BackEdges.begin(); it != BackEdges.end(); it++){
    auto* BBI = FRG->getBasicBlockHelperInfo(it->first);
    BBI->BackEdgeSet.insert(it->second);
  }
}

FieldReferenceGraph* CloseProximityBuilder::buildFieldReferenceGraph(const Function* F)
{
  DEBUG_WITH_TYPE(DEBUG_TYPE_FRG, dbgs() << "Create a new empty FRG\n");
  auto* FRG = new FieldReferenceGraph(F);
  FRGArray.push_back(FRG);
  // Create and connect node inside each basic block
  for (auto &BB : *F){
    DEBUG_WITH_TYPE(DEBUG_TYPE_FRG, dbgs() << "Build partial FRG for BB: " << BB << "\n");
    auto* BBI = FRG->createBasicBlockHelperInfo(&BB);
    for (auto &I : BB){
      if (auto FieldNum = StructInfo->getAccessFieldNum(&I)){
        // Case that I is a struct access
        DEBUG_WITH_TYPE(DEBUG_TYPE_FRG, dbgs() << "Found an instruction " << I << " is a struct access on field [" << FieldNum.getValue() << "]\n");
        auto* NewNode = FRG->createNewNode(FieldNum.getValue(), getMemAccessDataSize(&I));
        if (BBI->LastNode){
          DEBUG_WITH_TYPE(DEBUG_TYPE_FRG, dbgs() << "Previous nodes found in the BB\n");
          auto C = 0;
          if (auto ExCnt = StructInfo->getExecutionCount(&I))
            C = ExCnt.getValue();
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
  // Before connect nodes, need to mark all back edges first
  detectBackEdges(FRG, F);
  // Connect nodes between different basic blocks
  for (auto &BB : *F){
    auto* BBI = FRG->getBasicBlockHelperInfo(&BB);
    auto* Term = BB.getTerminator();
    for (const auto *SB : Term->successors()){
      auto* SBI = FRG->getBasicBlockHelperInfo(SB);
      ExecutionCountType C = 0;
      auto BBCount = StructInfo->getExecutionCount(&BB);
      auto SBCount = StructInfo->getExecutionCount(SB);
      if (BBCount.hasValue() && SBCount.hasValue()){
        // Take probability of the branch
        auto Prob = StructManager->getBranchProbability(&BB, SB);
        assert(Prob.hasValue());
        C = std::min(BBCount.getValue() * Prob.getValue(), SBCount.getValue() * 1.0);
      }
      if (!DisableIgnoreZeroCountNodes && C < 1e-3)
        continue;
      auto D = BBI->RemainBytes; // Use size of remaining data in BB
      if (BBI->BackEdgeSet.find(SB) != BBI->BackEdgeSet.end())
        FRG->connectNodes(BBI->LastNode, SBI->FirstNode, C, D, true);
      else
        FRG->connectNodes(BBI->LastNode, SBI->FirstNode, C, D);
    }
  }
  assert(StructInfo->getExecutionCount(&F->getEntryBlock()));
  auto* BBI = FRG->getBasicBlockHelperInfo(&F->getEntryBlock());
  FRG->setRootNode(BBI->FirstNode);
  DEBUG(dbgs() << "---------- FRG for function " << F->getName() << "----------------\n");
  DEBUG(FRG->debugPrint(dbgs()));
  DEBUG(dbgs() << "------------------------------------------------------------------------\n");
  return FRG;
}

void CloseProximityBuilder::updateCPG(FieldNumType Src, FieldNumType Dest, ExecutionCountType C, DataBytesType D)
{
  assert(Src <= NumElements && Dest <= NumElements);
  if (Src == 0 || Dest == 0 || C < 1e-3 || Src == Dest)
    // Give up update if from or to a dummy node, or if the count is zero
    return;
  assert(!std::isnan(C));
  if (Src != 0 && Dest != 0){
    if (Src > Dest){
      std::swap(Src, Dest);
    }
     DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Gonna update CPG between " << Src << " and " << Dest << " with count " << DEBUG_PRINT_COUNT(C) << " and distance " << DEBUG_PRINT_DIST(D) << ":\n");
    DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << " Before update: " << "(" << DEBUG_PRINT_COUNT(CloseProximityTable[Src-1][Dest-1].first) << "," << DEBUG_PRINT_DIST(CloseProximityTable[Src-1][Dest-1].second) << ")\n");
    assert(CloseProximityTable[Src-1][Dest-1].first + C != 0);
    updatePairByMerging(CloseProximityTable[Src-1][Dest-1].first, CloseProximityTable[Src-1][Dest-1].second, C, D);
    if (std::isnan(CloseProximityTable[Src-1][Dest-1].first) || std::isnan(CloseProximityTable[Src-1][Dest-1].second)){
      errs() << "Update CPG between " << Src << " and " << Dest << " with count " << DEBUG_PRINT_COUNT(C) << " and distance " << DEBUG_PRINT_DIST(D) << "\n";
    }
    DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << " After update: " << "(" << DEBUG_PRINT_COUNT(CloseProximityTable[Src-1][Dest-1].first) << "," << DEBUG_PRINT_DIST(CloseProximityTable[Src-1][Dest-1].second) << ")\n");
  }
}

void CloseProximityBuilder::updateGoldCPG(FieldNumType Src, FieldNumType Dest, ExecutionCountType C, DataBytesType D)
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
    DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << " After update: " << "(" << DEBUG_PRINT_COUNT(GoldCPT[Src-1][Dest-1].first) << "," << DEBUG_PRINT_DIST(GoldCPT[Src-1][Dest-1].second) << ")\n");
  }
}

void CloseProximityBuilder::updateCPGBetweenNodes(FieldReferenceGraph::Node* From, FieldReferenceGraph::Node* To, FieldReferenceGraph::Edge* Arc, ExecutionCountType C, DataBytesType D, NodeSetType* CheckList)
{
  if (To == NULL || To == From || CheckList->find(To) != CheckList->end())
    return;
  DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Update CPG from " << From->Id << " to " << To->Id << " with " << DEBUG_PRINT_COUNT(C) << " and " << DEBUG_PRINT_DIST(D) << "\n");
  assert(!std::isnan(C));
  updateCPG(From->FieldNum, To->FieldNum, C, D);
  CheckList->insert(To);
  for (auto* E : To->OutEdges){
    if (E->ToNode == From)
      continue;
    if (E->Collapsed){
      //DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Update CPG from " << From->Id << " to collapsed subtree edge (" << E->FromNode->Id << "," << E->ToNode->Id << ") with ratio " << Arc->ExecutionCount/To->InSum << "\n");
      DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Update CPG from " << From->Id << " to collapsed subtree edge (" << E->FromNode->Id << "," << E->ToNode->Id << ") with ratio " << E->ExecutionCount/To->OutSum << "\n");
      for (auto* Entry : E->CollapsedEntries){
        auto ExCnt = C;
        auto Dist = D;
        assert(To->InSum > 0);
        //updatePairByConnecting(ExCnt, Dist, Entry->ExecutionCount, Arc->ExecutionCount/To->InSum, Dist, To->Size + Entry->DataSize);
        updatePairByConnecting(ExCnt, E->ExecutionCount/To->OutSum, Dist, Entry->ExecutionCount, Arc->ExecutionCount/To->InSum, To->Size + Entry->DataSize);
        assert(!std::isnan(ExCnt));
        updateCPG(From->FieldNum, Entry->FieldNum, ExCnt, Dist);
      }
    }
    else{
      DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Found non-collapsed successor " << E->ToNode->Id << "\n");
      auto ExCnt = C;
      auto Dist = D;
      assert(To->OutSum > 0);
      updatePairByConnecting(ExCnt, E->ExecutionCount/To->OutSum, Dist, ExCnt, 1.0, To->Size + E->DataSize);
      if (ExCnt > 1e-3)
        updateCPGBetweenNodes(From, E->ToNode, E, ExCnt, Dist, CheckList);
    }
  }
  CheckList->erase(To);
}

void CloseProximityBuilder::updateCPGFromNodeToSubtree(FieldReferenceGraph::Edge* E)
{
  if (E->FromNode->FieldNum == 0 || E->ExecutionCount <= 1e-3)
    return;
  DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Update CPG from " << E->FromNode->Id << " to subtree " << E->ToNode->Id << "\n");
  NodeSetType CheckList;
  updateCPGBetweenNodes(E->FromNode, E->ToNode, E, E->ExecutionCount, E->DataSize, &CheckList);
}

bool CloseProximityBuilder::collapseSuccessor(FieldReferenceGraph* FRG, FieldReferenceGraph::Edge* Arc)
{
  bool Ret = false;
  DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Collapse successor Edge (" << Arc->FromNode->Id << "," << Arc->ToNode->Id << ")\n");
  assert(!std::isnan(Arc->ExecutionCount));
  updateCPG(Arc->FromNode->FieldNum, Arc->ToNode->FieldNum, Arc->ExecutionCount, Arc->DataSize);
  FRG->collapseNodeToEdge(Arc->ToNode, Arc);
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
        assert(!std::isnan(Entry->ExecutionCount));
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

bool CloseProximityBuilder::collapseRoot(FieldReferenceGraph* FRG, FieldReferenceGraph::Node* Root)
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
      }
      else if (E->ToNode->InEdges.size() == 1 && !E->Collapsed){
        // if SUCC can be collapsed, i.e. only has one predecessor
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

void CloseProximityBuilder::createCloseProximityRelations(FieldReferenceGraph* FRG, FieldReferenceGraph::Node* Root)
{
  DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Finalize Node " << Root->Id << " as root\n");
  // Traverse the remaining FRG and calculate CP relations from bottom of the FRG
  for (auto *E : Root->OutEdges){
    if (!E->LoopArc && !E->Collapsed && !E->ToNode->Visited)
      createCloseProximityRelations(FRG, E->ToNode);
  }
  // Calculate CP relations between each node and its non-collapsed successors
  DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Calculate CP-relations with Node " << Root->Id << " as root\n");
  for (auto *E : Root->OutEdges){
    if (!E->Collapsed){
      DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Found a non-collapsed edge: (" << E->FromNode->Id << "," << E->ToNode->Id << ")\n");
      updateCPGFromNodeToSubtree(E);
    }
  }
  Root->Visited = true;
}

void CloseProximityBuilder::collapseFieldReferenceGraph(FieldReferenceGraph* FRG)
{
  auto* Root = FRG->getRootNode();
  collapseRoot(FRG, Root);
  // If the FRG is not collapsed to a single node, need calculate CP relations of remaining nodes with brutal force
  auto AllCollapsed = true;
  for (auto* E : Root->OutEdges){
    if (!E->Collapsed)
      AllCollapsed = false;
  }
  // FIXME: Use this assertion to detect if this happens.
  if (AllCollapsed)
    return;
  createCloseProximityRelations(FRG, Root);
}

// Functions for building CPG with brutal force, used for debugging
void CloseProximityBuilder::calculateCPRelation(EdgeArrayType* Path)
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
    updatePairByConnecting(C, E->ExecutionCount/E->FromNode->OutSum, D, E->ExecutionCount, 1.0, E->DataSize + E->FromNode->Size);
  }
  updateGoldCPG((*Path)[0]->FromNode->FieldNum, (*Path)[Path->size()-1]->ToNode->FieldNum, C, D);
}

void CloseProximityBuilder::findPathInFRG(FieldReferenceGraph::Node* Start, FieldReferenceGraph::Node* End, EdgeArrayType* Path, NodeSetType* VisitedNodes)
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

void CloseProximityBuilder::calculatePathBetween(FieldReferenceGraph::Node* FromNode, FieldReferenceGraph::Node* ToNode)
{
  EdgeArrayType Path;
  NodeSetType VisitedNodes;
  DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Calculate Path between " << FromNode->Id << " and " << ToNode->Id << "\n");
  assert(FromNode != ToNode);
  VisitedNodes.insert(FromNode);
  findPathInFRG(FromNode, ToNode, &Path, &VisitedNodes);
}

void CloseProximityBuilder::createFRGPairs(FieldReferenceGraph* FRG, FieldReferenceGraph::Node* Root, EdgeArrayType* Path)
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

void CloseProximityBuilder::createGoldCloseProximityRelations(FieldReferenceGraph* FRG)
{
  auto* Root = FRG->getRootNode();
  EdgeArrayType Path;
  DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Create CP Relations for FRG with brutal force\n");
  createFRGPairs(FRG, Root, &Path);
}

void CloseProximityBuilder::compareCloseProximityRelations() const{
  for (unsigned i = 0; i < NumElements; i++){
    for (unsigned j = i+1; j < NumElements; j++){
      if (std::abs(GoldCPT[i][j].first - CloseProximityTable[i][j].first) / GoldCPT[i][j].first > 0.1 ||
          std::abs(GoldCPT[i][j].second - CloseProximityTable[i][j].second) / GoldCPT[i][j].second > 0.1){
        outs() << "Found error in CPG: F" << i+1 << " and F" << j+1 << " should be (" << DEBUG_PRINT_COUNT(GoldCPT[i][j].first) << "," << DEBUG_PRINT_DIST(GoldCPT[i][j].second) << ") but calculated as (" << DEBUG_PRINT_COUNT(CloseProximityTable[i][j].first) << "," << DEBUG_PRINT_DIST(CloseProximityTable[i][j].second) << ")\n";
      }
    }
  }
}

void CloseProximityBuilder::buildCloseProximityRelations()
{
  for (auto& F : CurrentModule){
    if (F.isDeclaration())
      continue;
    if (!F.getEntryCount() || F.getEntryCount().getValue() == 0)
      continue;
    if (!StructInfo->isFunctionToAnalyze(&F))
      continue;
    DEBUG_WITH_TYPE(DEBUG_TYPE_CPG, dbgs() << "Analyzing function " << F.getName() << "\n");
    auto* FRG = buildFieldReferenceGraph(&F);
    assert(FRG);
    if (PerformFRGOnly)
      continue;
    // Brutal force get CP relations, should perform first because it preserves FRG
    if (PerformCPGCheck)
      createGoldCloseProximityRelations(FRG);
    // Collpase FRG and get CPG
    collapseFieldReferenceGraph(FRG);
  }
  if (PerformCPGCheck)
    compareCloseProximityRelations();
}

void CloseProximityBuilder::debugPrintFieldReferenceGraph(raw_ostream& OS) const
{
  for (auto *FRG : FRGArray){
    FRG->debugPrint(OS);
  }
}

void CloseProximityBuilder::debugPrintCloseProximityGraph(raw_ostream& OS) const
{
  if (PerformFRGOnly){
    debugPrintFieldReferenceGraph(OS);
    return;
  }
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
  if (PerformCPGCheck)
    debugPrintGoldCPT(OS);
}

void CloseProximityBuilder::debugPrintGoldCPT(raw_ostream& OS) const
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
