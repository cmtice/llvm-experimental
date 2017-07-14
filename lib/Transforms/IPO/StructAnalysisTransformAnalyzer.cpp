// lib/Tranforms/IPO/StructAnalysisTransformAnalyzer.cpp - Performs Cache-Aware
// Structure Analysis-*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===------------------------------------------------------------------------===//
//
// This file implements two StructTransformAnalyzer that are used to give suggestions
// on field reordering and struct splitting
//
//===------------------------------------------------------------------------===//

#include "StructFieldCacheAnalysisImpl.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/Support/Format.h"

#define DEBUG_TYPE_REORDER "struct-analysis-reorder"
#define DEBUG_TYPE_SPLIT "struct-analysis-split"

#define DEBUG_PRINT_COUNT(x) (format("%.2f", (x)))
#define DEBUG_PRINT_DIST(x) (format("%.3f", (x)))

// Parameters for tuning analyzers
static cl::opt<unsigned> CacheBlockSize(
    "struct-analysis-cache-block-size", cl::init(64), cl::Hidden,
    cl::desc("Set cache block size for the analysis"));

// Parameter tuning for struct split
static cl::opt<unsigned> StructSplitColdRatio(
    "struct-analysis-split-cold-ratio", cl::init(4), cl::Hidden,
    cl::desc("Set COLD_RATIO in struct split algorithm"));

static cl::opt<unsigned> StructSplitDistanceThreshold(
    "struct-analysis-split-distance-threshold", cl::init(8), cl::Hidden,
    cl::desc("Set DISTANCE_THRESHOLD in struct split algorithm"));

static cl::opt<unsigned> StructSplitMaxSize(
    "struct-analysis-split-max-size", cl::init(32), cl::Hidden,
    cl::desc("Set MAX_SIZE in struct split algorithm"));

static cl::opt<unsigned> StructSplitSizePenalty(
    "struct-analysis-split-size-penalty", cl::init(2), cl::Hidden,
    cl::desc("Set SIZE_PENALTY in struct split algorithm"));

// Functions for StructTransformAnalyzer
StructTransformAnalyzer::StructTransformAnalyzer(const Module& CM, const StructType* ST, const CloseProximityBuilder* CPB, const DICompositeType* DI) : StructureType(ST), CPBuilder(CPB), NumElements(StructureType->getNumElements()), DebugInfo(DI), Eligibility(true)
{
  if (NumElements <= 2){
    Eligibility = false;
  }
  FieldSizes.resize(NumElements);
  for (unsigned i = 0; i < NumElements; i++){
    FieldSizes[i] = CM.getDataLayout().getTypeSizeInBits(StructureType->getElementType(i)) / 8;
  }
}

void StructTransformAnalyzer::mapFieldsToDefinition()
{
  FieldDI.resize(NumElements);
  if (DebugInfo){
    assert(DebugInfo->getElements().size() <= NumElements);
    FieldNumType FieldNum = 0;
    for (unsigned i = 0; i < DebugInfo->getElements().size(); i++){
      DINode* node = DebugInfo->getElements()[i];
      assert(node && isa<DIDerivedType>(node) && dyn_cast<DIDerivedType>(node)->getTag() == dwarf::Tag::DW_TAG_member);
      Metadata* T = dyn_cast<DIDerivedType>(node)->getBaseType();
      assert(T && isa<DIType>(T));
      auto Size = dyn_cast<DIType>(T)->getSizeInBits()/8 != FieldSizes[FieldNum];
      if (Size == 0){
        if (dyn_cast<DIType>(T)->getTag() == dwarf::Tag::DW_TAG_structure_type){
          assert(isa<DICompositeType>(T));
        }
      }
      while (Size && FieldNum < NumElements){
        DEBUG_WITH_TYPE(DEBUG_TYPE_REORDER, dbgs() << "Field " << FieldNum+1 << " might be a padding because its size is " << FieldSizes[FieldNum] << " Bytes but the actual field is " << dyn_cast<DIType>(T)->getSizeInBits()/8 << " Bytes\n");
        FieldDI[FieldNum] = NULL;
        FieldNum++;
      }
      assert(FieldNum < NumElements);
      assert(dyn_cast<DIType>(T)->getSizeInBits()/8 == FieldSizes[FieldNum]);
      DEBUG_WITH_TYPE(DEBUG_TYPE_REORDER, dbgs() << *dyn_cast<DIType>(T) << "\n");
      FieldDI[FieldNum] = new FieldDebugInfo(i, dyn_cast<DIDerivedType>(node)->getName(), dyn_cast<DIType>(T)->getName());
      FieldNum++;
    }
  }
  else{
    unsigned Address;
    unsigned FieldNum = 0;
    for (unsigned i = 0; i < NumElements; i++){
      auto* Ty = StructureType->getElementType(i);
      if (!Ty->isArrayTy()){
        FieldDI[i] = new FieldDebugInfo(FieldNum++);
        Address += FieldSizes[i];
      }
      else{
        //ArrayTy is suspicious to be a padding
        if ((i < NumElements-1 && FieldSizes[i] > FieldSizes[i+1]) ||
            (i == NumElements-1 && FieldSizes[i] > FieldSizes[i-1]))
          // Padding can't be larger than the field before or after
          FieldDI[i] = new FieldDebugInfo(FieldNum++);
        else if ((i < NumElements-1 && Address % FieldSizes[i+1] != 0 && (Address + FieldSizes[i]) % FieldSizes[i+1] == 0) ||
                 (i == NumElements-1 && Address % FieldSizes[i-1] != 0 && (Address + FieldSizes[i]) % FieldSizes[i-1] == 0)){
          // If remove this field, the next/previous field is not aligned and add this field, it is aligned, it's likely to be a padding
          unsigned MaxWCP = 0;
          dbgs() << "Considering F" << i+1 << "\n";
          for (unsigned j = 0; j < NumElements; j++){
            auto* Pair = CPBuilder->getCloseProximityPair(i, j);
            dbgs() << "(" << Pair->first << "," << Pair->second << ")\n";
            if (Pair->first > MaxWCP)
              MaxWCP = Pair->first;
          }
          if (MaxWCP != 0)
            FieldDI[i] = new FieldDebugInfo(FieldNum++);
          else
            // This is highly likely to be a padding because there's no WCP with other fields
            FieldDI[i] = NULL;
        }
        else{
          FieldDI[i] = new FieldDebugInfo(FieldNum++);
        }
        Address += FieldSizes[i];
      }
    }
  }
}

// Functions for FieldReorderAnalyzer
FieldReorderAnalyzer::FieldReorderAnalyzer(const Module& CM, const StructType* ST, const CloseProximityBuilder* CPB, const DICompositeType* DI) : StructTransformAnalyzer(CM, ST, CPB, DI)
{
  DEBUG_WITH_TYPE(DEBUG_TYPE_REORDER, dbgs() << "Initializing FieldReorderAnalyzer of size: " << NumElements << "\n");
  CloseProximityRelations.resize(NumElements);
  FieldsToReorder.clear();
  double MaxWCP = 0;
  FieldNumType MaxI, MaxJ;
  for (unsigned i = 0; i < NumElements; i++){
    FieldsToReorder.insert(i);
    CloseProximityRelations[i].resize(NumElements);
    for (unsigned j = 0; j < NumElements; j++){
      CloseProximityRelations[i][j] = calculateCloseProximity(i, j);
      if (i < j && CloseProximityRelations[i][j] > MaxWCP){
        MaxWCP = CloseProximityRelations[i][j];
        MaxI = i;
        MaxJ = j;
      }
    }
  }
  if (MaxWCP < 1e-3){
    Eligibility = false;
    return;
  }
  DEBUG(dbgs() << "CPG for field reordering recommendation\n");
  for (unsigned i = 0; i < NumElements; i++){
    DEBUG(dbgs() << "F" << i+1 << "\t");
    for (unsigned j = 0; j < NumElements; j++)
      DEBUG(dbgs() << DEBUG_PRINT_COUNT(CloseProximityRelations[i][j]) << "\t");
    DEBUG(dbgs() << "\n");
  }

  mapFieldsToDefinition();
  // FIXME: Need to reconsider if the order of the pair makes differences
  NewOrder.push_back(MaxI);
  NewOrder.push_back(MaxJ);
  FieldsToReorder.erase(MaxI);
  FieldsToReorder.erase(MaxJ);

  for (unsigned i = 0; i < NumElements; i++){
    if (FieldDI[i] == NULL){
      outs() << "Field " << i+1 << " is actually a padding, ignore in reordering...\n";
      FieldsToReorder.erase(i);
    }
  }

  DEBUG_WITH_TYPE(DEBUG_TYPE_REORDER, dbgs() << "Use F" << MaxI+1 << " and F" << MaxJ+1 << " as seeds in reordering\n");
  assert(MaxWCP == getWCP());
  DEBUG_WITH_TYPE(DEBUG_TYPE_REORDER, dbgs() << "Current WCP: " << DEBUG_PRINT_COUNT(MaxWCP) << "\n");
}

double FieldReorderAnalyzer::calculateCloseProximity(FieldNumType Field1, FieldNumType Field2){
  if (Field1 == Field2)
    return 0;
  if (!canFitInOneCacheBlock(Field1, Field2))
    return 0;
  auto* Pair = CPBuilder->getCloseProximityPair(Field1, Field2);
  if (Pair->first < 1e-3)
    return 0;
  if (Pair->second < 1)
    return Pair->first;
  return Pair->first / Pair->second;
}

bool FieldReorderAnalyzer::canFitInOneCacheBlock(FieldNumType Field1, FieldNumType Field2) const
{
  if (FieldSizes[Field1] % CacheBlockSize == 0)
    return false;
  return true;
}

double FieldReorderAnalyzer::calculateWCPWithinCacheBlock(std::vector<FieldNumType>* CacheBlock) const
{
  double WCP = 0;
  DEBUG_WITH_TYPE(DEBUG_TYPE_REORDER, dbgs() << "Calculate WCP within a cache block: ");
  for (unsigned i = 0; i < CacheBlock->size(); i++){
    DEBUG_WITH_TYPE(DEBUG_TYPE_REORDER, dbgs() << "F" << (*CacheBlock)[i]+1 << " ");
    for (unsigned j = i+1; j < CacheBlock->size(); j++)
      if ((*CacheBlock)[i] < (*CacheBlock)[j])
        WCP += CloseProximityRelations[(*CacheBlock)[i]][(*CacheBlock)[j]];
      else
        WCP += CloseProximityRelations[(*CacheBlock)[j]][(*CacheBlock)[i]];
  }
  DEBUG_WITH_TYPE(DEBUG_TYPE_REORDER, dbgs() << "produces WCP: " << DEBUG_PRINT_COUNT(WCP) << "\n");
  return WCP;
}

double FieldReorderAnalyzer::getWCP() const
{
  unsigned CurrentCacheBlockSize = 0;
  std::vector<FieldNumType> CurrentCacheBlock;
  double WCP = 0;
  for (auto& FieldNum : NewOrder){
    auto Size = FieldSizes[FieldNum];
    DEBUG_WITH_TYPE(DEBUG_TYPE_REORDER, dbgs() << "Field " << FieldNum+1 << " size is " << Size << "\n");
    if (!StructureType->getElementType(FieldNum)->isAggregateType() && CurrentCacheBlockSize % Size){
      // Alignment needed, need to add padding. Ignore alignment for aggregated type (array and struct)
      DEBUG_WITH_TYPE(DEBUG_TYPE_REORDER, dbgs() << "Need to add " << (CurrentCacheBlockSize / Size + 1) * Size - CurrentCacheBlockSize << " Bytes of padding\n");
      CurrentCacheBlockSize = (CurrentCacheBlockSize / Size + 1) * Size;
      assert(CurrentCacheBlockSize <= CacheBlockSize);
    }
    if (CurrentCacheBlockSize == CacheBlockSize){
      DEBUG_WITH_TYPE(DEBUG_TYPE_REORDER, dbgs() << "Need to put into a new cache block calculate current one\n");
      WCP += calculateWCPWithinCacheBlock(&CurrentCacheBlock);
      CurrentCacheBlockSize = 0;
      CurrentCacheBlock.clear();
    }
    else{
      CurrentCacheBlockSize += Size;
      CurrentCacheBlock.push_back(FieldNum);
      if (CurrentCacheBlockSize >= CacheBlockSize){
        DEBUG_WITH_TYPE(DEBUG_TYPE_REORDER, dbgs() << "Current cache block cannot fit remaining field\n");
        WCP += calculateWCPWithinCacheBlock(&CurrentCacheBlock);
        CurrentCacheBlockSize -= CacheBlockSize;
        CurrentCacheBlock.clear();
        if (CurrentCacheBlockSize > 0)
          CurrentCacheBlock.push_back(FieldNum);
      }
    }
  }
  WCP += calculateWCPWithinCacheBlock(&CurrentCacheBlock);
  return WCP;
}

double FieldReorderAnalyzer::estimateWCPAtBack(FieldNumType FieldNum)
{
  NewOrder.push_back(FieldNum);
  auto WCP = getWCP();
  NewOrder.pop_back();
  return WCP;
}

double FieldReorderAnalyzer::estimateWCPAtFront(FieldNumType FieldNum)
{
  NewOrder.push_front(FieldNum);
  auto WCP = getWCP();
  NewOrder.pop_front();
  return WCP;
}

void FieldReorderAnalyzer::makeSuggestions()
{
  if (!Eligibility){
    outs() << "Struct has too few fields or cold in profiling for reordering\n";
    return;
  }

  double MaxWCP = getWCP();
  while (FieldsToReorder.size()){
    FieldNumType MaxField;
    bool InFront = false;
    MaxWCP = 0;
    for (auto& F : FieldsToReorder){
      auto WCP = estimateWCPAtBack(F);
      if (WCP > MaxWCP){
        DEBUG_WITH_TYPE(DEBUG_TYPE_REORDER, dbgs() << "Found better ordering by add F" << F+1 << " in the end\n");
        DEBUG_WITH_TYPE(DEBUG_TYPE_REORDER, dbgs() << "Current WCP: " << DEBUG_PRINT_COUNT(WCP) << "\n");
        MaxWCP = WCP;
        MaxField = F;
        InFront = false;
      }
      WCP = estimateWCPAtFront(F);
      if (WCP > MaxWCP){
        DEBUG_WITH_TYPE(DEBUG_TYPE_REORDER, dbgs() << "Found better ordering by add F" << F+1 << " in the front\n");
        DEBUG_WITH_TYPE(DEBUG_TYPE_REORDER, dbgs() << "Current WCP: " << DEBUG_PRINT_COUNT(WCP) << "\n");
        MaxWCP = WCP;
        MaxField = F;
        InFront = true;
      }
    }
    if (InFront)
      NewOrder.push_front(MaxField);
    else
      NewOrder.push_back(MaxField);
    FieldsToReorder.erase(MaxField);
  }

  // Print suggestions
  std::list<FieldNumType> BestOrder(NewOrder);
  NewOrder.clear();
  for (unsigned i = 0; i < NumElements; i++){
    if (FieldDI[i])
      NewOrder.push_back(i);
  }
  auto OldWCP = getWCP();
  DEBUG_WITH_TYPE(DEBUG_TYPE_REORDER, dbgs() << "New WCP: " << DEBUG_PRINT_COUNT(MaxWCP) << " vs Old WCP: " << DEBUG_PRINT_COUNT(OldWCP) << "\n");
  assert(MaxWCP >= OldWCP || std::abs(OldWCP-MaxWCP)/OldWCP < 1e-3);
  auto Confidence = 100.0 * (MaxWCP - OldWCP) / OldWCP;
  if (Confidence > 100.0)
    Confidence = 100.0;
  if (Confidence < 1.0){
    outs() << "Suggest to keep the current order: \n";
    for (auto& FieldNum : NewOrder){
      assert(FieldDI[FieldNum]);
      outs() << "F" << FieldDI[FieldNum]->FieldNum+1 << " ";
    }
    outs() << "\n";
  }
  else{
    outs() << "Suggest to reorder the fields according to: ";
    for (auto& FieldNum : BestOrder){
      assert(FieldDI[FieldNum]);
      outs() << "F" << FieldDI[FieldNum]->FieldNum+1 << " ";
    }
    outs() << " might improve performance with " << format("%.0f", Confidence) << " % confident\n";
  }
}

// Functions for StructSplitAnalyzer
StructSplitAnalyzer::StructSplitAnalyzer(const Module& CM, const StructType* ST, const CloseProximityBuilder* CPB, const DICompositeType* DI) : StructTransformAnalyzer(CM, ST, CPB, DI), Params({StructSplitColdRatio, StructSplitDistanceThreshold, StructSplitMaxSize, StructSplitSizePenalty})
{
  DEBUG_WITH_TYPE(DEBUG_TYPE_SPLIT, dbgs() << "Initializing StructSplitAnalyzer of size: " << NumElements << "\n");
  CloseProximityRelations.resize(NumElements);
  for (unsigned i = 0; i < NumElements; i++){
    CloseProximityRelations[i].resize(NumElements);
    FieldsToRegroup.insert(i);
    for (unsigned j = 0; j < NumElements; j++){
      CloseProximityRelations[i][j] = calculateCloseProximity(i, j);
    }
  }
  DEBUG(dbgs() << "Parameters used in struct spliting: \n");
  DEBUG(dbgs() << "COLD_RATIO: " << Params.ColdRatio << "\n");
  DEBUG(dbgs() << "DISTANCE_THRESHOLD: " << Params.DistanceThreshold << "\n");
  DEBUG(dbgs() << "MAX_SIZE: " << Params.MaxSize << "\n");
  DEBUG(dbgs() << "SIZE_PENALTY: " << Params.SizePenalty << "\n");
  DEBUG(dbgs() << "CPG for struct split recommendation\n");
  for (unsigned i = 0; i < NumElements; i++){
    DEBUG(dbgs() << "F" << i+1 << "\t");
    for (unsigned j = 0; j < NumElements; j++)
      DEBUG(dbgs() << DEBUG_PRINT_COUNT(CloseProximityRelations[i][j]) << "\t");
    DEBUG(dbgs() << "\n");
  }
  mapFieldsToDefinition();
  for (unsigned i = 0; i < NumElements; i++){
    if (FieldDI[i] == NULL){
      DEBUG(dbgs() << "Remove F" << i+1 << " from fields to regroup.\n");
      FieldsToRegroup.erase(i);
    }
  }
  DEBUG_WITH_TYPE(DEBUG_TYPE_SPLIT, dbgs() << "Finish constructor\n");
}

double StructSplitAnalyzer::calculateCloseProximity(FieldNumType Field1, FieldNumType Field2)
{
  auto* Pair = CPBuilder->getCloseProximityPair(Field1, Field2);
  if (Pair->second > Params.DistanceThreshold)
    return 0;
  if (FieldSizes[Field1] + FieldSizes[Field2] > Params.MaxSize)
    return 0;
  auto Ratio = 0;
  if (FieldSizes[Field1] > FieldSizes[Field2])
    Ratio = FieldSizes[Field1] / FieldSizes[Field2];
  else
    Ratio = FieldSizes[Field2] / FieldSizes[Field1];
  Ratio *= Params.SizePenalty;
  return Pair->first / Ratio;
}

FieldPairType StructSplitAnalyzer::findMaxRemainCP() const
{
  double MaxCP = 0;
  auto it = FieldsToRegroup.begin();
  auto Field1 = *(it++);
  assert(it != FieldsToRegroup.end());
  auto Field2 = *it;
  DEBUG_WITH_TYPE(DEBUG_TYPE_SPLIT, dbgs() << "Try to find a pair with maxmium CP\n");
  for (auto& F1 : FieldsToRegroup)
    for (auto& F2 : FieldsToRegroup)
      if (F1 != F2){
        if (CloseProximityRelations[F1][F2] > MaxCP){
          MaxCP = CloseProximityRelations[F1][F2];
          Field1 = F1;
          Field2 = F2;
        }
      }
  DEBUG_WITH_TYPE(DEBUG_TYPE_SPLIT, dbgs() << "The pair with largest CP is: (F" << Field1+1 << ", F" << Field2+1 << ")\n");
  return std::make_pair(Field1, Field2);
}

void StructSplitAnalyzer::makeSuggestions()
{
  if (!Eligibility){
    outs() << "Struct has too few fields or cold in profiling for splitting\n";
    return;
  }
  DEBUG_WITH_TYPE(DEBUG_TYPE_SPLIT, dbgs() << "Making suggestions for struct splitting\n");
  while (FieldsToRegroup.size()){
    auto* NewRecord = new SubRecordType;
    SubRecords.push_back(NewRecord);
    DEBUG_WITH_TYPE(DEBUG_TYPE_SPLIT, dbgs() << "Create a new empty record\n");
    if (FieldsToRegroup.size() == 1){
      DEBUG_WITH_TYPE(DEBUG_TYPE_SPLIT, dbgs() << "Only have one remaining field...\n");
      NewRecord->push_back(*FieldsToRegroup.begin());
      FieldsToRegroup.clear();
      break;
    }
    auto BestPair = findMaxRemainCP();
    double Threshold = CloseProximityRelations[BestPair.first][BestPair.second] / Params.ColdRatio;
    if (Threshold == 0){
      // All the remaining fields are cold with others, create record for each for them
      DEBUG_WITH_TYPE(DEBUG_TYPE_SPLIT, dbgs() << "All remaining fields are cold, create separate record for each\n");
      DEBUG_WITH_TYPE(DEBUG_TYPE_SPLIT, dbgs() << "Put F" << BestPair.first+1 << " into a new record\n");
      NewRecord->push_back(BestPair.first);
      FieldsToRegroup.erase(BestPair.first);
      for (auto& F : FieldsToRegroup){
        DEBUG_WITH_TYPE(DEBUG_TYPE_SPLIT, dbgs() << "Put F" << F+1 << " into a new record\n");
        NewRecord = new SubRecordType;
        SubRecords.push_back(NewRecord);
        NewRecord->push_back(F);
      }
      FieldsToRegroup.clear();
      break;
    }
    DEBUG_WITH_TYPE(DEBUG_TYPE_SPLIT, dbgs() << "CP value is: " << DEBUG_PRINT_COUNT(CloseProximityRelations[BestPair.first][BestPair.second]) << " and threshold is: " << DEBUG_PRINT_COUNT(Threshold) << "\n");
    // Add the pair into the new sub record and remove from remaining fields
    NewRecord->push_back(BestPair.first);
    NewRecord->push_back(BestPair.second);
    auto NewRecordSize = FieldSizes[BestPair.first] + FieldSizes[BestPair.second];
    FieldsToRegroup.erase(BestPair.first);
    FieldsToRegroup.erase(BestPair.second);
    // If the pair is enough to take the MAX_SIZE, no need to add more fields to the subrecord
    if (NewRecordSize >= Params.MaxSize)
      continue;
    while (FieldsToRegroup.size()){
      double MaxSum = 0;
      FieldNumType MaxF = 0;
      for (auto& Field : FieldsToRegroup){
        // If the field is too large to fit in the new sub record, give up on this one
        if (FieldSizes[Field] + NewRecordSize > Params.MaxSize)
          continue;
        auto Sum = 0;
        for (auto& Fi : *NewRecord){
          Sum += CloseProximityRelations[Field][Fi];
        }
        if (Sum > MaxSum){
          MaxSum = Sum;
          MaxF = Field;
        }
      }
      // Take average
      MaxSum /= NewRecord->size();
      DEBUG_WITH_TYPE(DEBUG_TYPE_SPLIT, dbgs() << "F" << MaxF+1 << " has the best CP relations with new record: " << DEBUG_PRINT_COUNT(MaxSum) << "\n");
      if (MaxSum < Threshold)
        // None of the remaining fields is suitable to put into this sub record
        break;
      DEBUG_WITH_TYPE(DEBUG_TYPE_SPLIT, dbgs() << "F" << MaxF+1 << " is added to the new record\n");
      NewRecord->push_back(MaxF);
      NewRecordSize += FieldSizes[MaxF];
      FieldsToRegroup.erase(MaxF);
    }
  }
  assert(FieldsToRegroup.size() == 0);
  for (unsigned i = 0; i < SubRecords.size(); i++){
    outs() << "Group #" << i+1 << ":{ ";
    for (auto& F : *SubRecords[i]){
      outs() << "F" << F+1 << " ";
    }
    outs() << "}\n";
  }
}
