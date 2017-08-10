// lib/Tranforms/IPO/StructTransformAnalyzer.cpp - Performs Cache-Aware
// Structure Analysis-*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===------------------------------------------------------------------------===//
//
// This file implements three classes derived from StructTransformAnalyzer that
// are used to give suggestions on field reordering and struct splitting.
//
// StructTransformAnalyzer is the base class that requires two virtual
// functions, makeSuggestions() and calculateCloseProximity() to be implemented
// in derived class because different suggestion algorithms should have
// different way to evaluate close proximity and different methods to make
// suggestions.
//
// FieldReorderAnalyzer and OldFieldReorderAnalyzer are two derived classes that
// implement two different reordering algorithms to give suggestions to reorder
// fields of a given struct. See the code for details of the differences between
// the two algorithms but generally, FieldReorderAnalyzer weighs cache behavior
// when reordering fields, while OldFieldReorderAnalyzer uses greedy algorithm
// to add fields first that generates potential benefits to the order.
//
// StructSplitAnalyzer is a derived class the implements a struct splitting
// algorithm. It has some parameters to tune (defined as static global variables
// at the beginning)
//
// The algorithms are derived from the following paper:
//  M. Hagog, C. Tice “Cache Aware Data Layout Reorganization Optimization
//  in GCC”, Proceedings of the GCC Developers’ Summit,  Ottawa, 2005.
//===------------------------------------------------------------------------===//

#include "StructFieldCacheAnalysisImpl.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/Support/Format.h"

#define DEBUG_TYPE_REORDER "struct-analysis-reorder"
#define DEBUG_TYPE_SPLIT "struct-analysis-split"

#define DEBUG_PRINT_COUNT(x) (format("%.2f", (x)))
#define DEBUG_PRINT_DIST(x) (format("%.3f", (x)))

// Parameters for tuning analyzers
static cl::opt<unsigned>
    CacheBlockSize("struct-analysis-cache-block-size", cl::init(64), cl::Hidden,
                   cl::desc("Set cache block size for the analysis"));

// Parameter tuning for struct split
static cl::opt<unsigned> StructSplitColdRatio(
    "struct-analysis-split-cold-ratio", cl::init(5), cl::Hidden,
    cl::desc("Set a ratio of the maximum CP relation of the current "
             "sub-struct; Below it means it is not benefiical to include the "
             "field into the current sub-struct"));

static cl::opt<unsigned> StructSplitDistanceThreshold(
    "struct-analysis-split-distance-threshold", cl::init(120), cl::Hidden,
    cl::desc("Set a threshold that if two fields have larger distance, they "
             "will be classified as no relations during the struct splitting "
             "algorithm"));

static cl::opt<unsigned> StructSplitMaxSize(
    "struct-analysis-split-max-size", cl::init(32), cl::Hidden,
    cl::desc("Set limit size of each sub-struct in struct split algorithm"));

static cl::opt<unsigned> StructSplitSizePenalty(
    "struct-analysis-split-size-penalty", cl::init(20), cl::Hidden,
    cl::desc("Set the penalty of two fields with different sizes in struct "
             "split algorithm"));

static cl::opt<unsigned> StructSplitMaxThresholdUpdates(
    "struct-analysis-split-max-threshold-updates", cl::init(3), cl::Hidden,
    cl::desc(
        "Max number of lowering threshold allowed when making suggestions"));

// Utility functions
// Function to check if a double variable is small enough to be considered
// close to zero
static bool isCloseToZero(double Number) { return (Number < 1e-3); }

// Functions of StructTransformAnalyzer
// Constructor of base class
StructTransformAnalyzer::StructTransformAnalyzer(
    const Module &CM, const StructType *ST, const CloseProximityBuilder *CPB,
    const DICompositeType *DI)
    : StructureType(ST), CPBuilder(CPB),
      NumElements(StructureType->getNumElements()), DebugInfo(DI),
      Eligibility(NumElements > 2) {
  // Calculate field sizes and establish a set of all fields considered for
  // transformation
  FieldSizes.resize(NumElements);
  for (unsigned i = 0; i < NumElements; i++) {
    FieldSizes[i] =
        CM.getDataLayout().getTypeSizeInBits(StructureType->getElementType(i)) /
        8;
    FieldsToTransform.insert(i);
  }

  // Check if any field could be a padding
  mapFieldsToDefinition();
  for (unsigned i = 0; i < NumElements; i++) {
    if (FieldDI[i] == nullptr) {
      outs() << "Field " << i + 1
             << " is actually a padding, ignore in suggestions...\n";
      FieldsToTransform.erase(i);
    }
  }
}

// Functions that maps LLVM struct types to struct definition in source
// code. The function constructs an array FieldDI, where each index represents
// each field in LLVM struct type and the content in each index is the debug
// info (FieldDebugInfo type) that contains info like name of the field,
// type of the field and actual field number (in case of padding). If a field
// is a padding, we save a nullptr to the according index.
// When DebugInfo of the struct is not retrieved (which is the
// default case for now), we use some heuristics to guess which field
// might be a padding. Other elements in FieldDI stores FieldDebugInfo type
// that only contains a field number in original definition (excluding padding)
void StructTransformAnalyzer::mapFieldsToDefinition() {
  FieldDI.resize(NumElements);
  assert(DebugInfo == nullptr);
// If there's DebugInfo for this struct retrieved, one could possibly go over
// all the fields in DebugInfo in this struct and for each field in DebugInfo,
// check if a field (FieldNum) in LLVM StructType has the same size.
// If so, the two will be the same field and create a new FieldDebugInfo with
// the information in DebugInfo and save to FieldDI[FieldNum]. If not, the field
// FieldNum might be a padding so save a nullptr to FieldDI and move on to check
// the next FieldNum
#if 0
  // Not actually using this part because DebugInfo can't be correctly
  // retrieved now; Also this code has never been executed and might be broken
  // leave here for future references
  assert(DebugInfo->getElements().size() <= NumElements);
  FieldNumType FieldNum = 0;
  for (unsigned i = 0; i < DebugInfo->getElements().size(); i++) {
    // Find a field in debug info and find a matching field in LLVM
    // struct type
    DINode *node = DebugInfo->getElements()[i];
    assert(node && isa<DIDerivedType>(node) &&
           dyn_cast<DIDerivedType>(node)->getTag() ==
           dwarf::Tag::DW_TAG_member);
    Metadata *T = dyn_cast<DIDerivedType>(node)->getBaseType();
    assert(T && isa<DIType>(T));
    auto Size = dyn_cast<DIType>(T)->getSizeInBits() / 8;
    // Check if the current size of FieldNum in LLVM definitions matches
    // the debug info definition
    while (Size != FieldSizes[FieldNum] && FieldNum < NumElements) {
      // The current FieldNum has to be an array to be a padding
      if (dyn_cast<DIType>(T)->getTag() ==
          dwarf::Tag::DW_TAG_structure_type) {
        assert(isa<DICompositeType>(T));
      }
      DEBUG_WITH_TYPE(DEBUG_TYPE,
                      dbgs() << "Field " << FieldNum + 1
                      << " might be a padding because its size is "
                      << FieldSizes[FieldNum]
                      << " Bytes but the actual field is " << Size
                      << " Bytes\n");
      // Mark this FieldNum as a padding
      FieldDI[FieldNum] = nullptr;
      // Move on the next FieldNum to check if we can find a match
      FieldNum++;
      Size = dyn_cast<DIType>(T)->getSizeInBits() / 8;
    }
    // There has to be a FieldNum that can match the size of current field
    // in debug info
    assert(FieldNum < NumElements);
    DEBUG_WITH_TYPE(DEBUG_TYPE_REORDER, dbgs()
                    << *dyn_cast<DIType>(T) << "\n");
    FieldDI[FieldNum] =
        new FieldDebugInfo(i, dyn_cast<DIDerivedType>(node)->getName(),
                           dyn_cast<DIType>(T)->getName());
    FieldNum++;
  }
#endif
  auto checkNoCPRelationWithOtherFields = [&](unsigned Idx) {
    unsigned MaxCP = 0;
    for (unsigned i = 0; i < NumElements; i++) {
      auto *Pair = CPBuilder->getCloseProximityPair(Idx, i);
      if (Pair->first > MaxCP)
        MaxCP = Pair->first;
    }
    return isCloseToZero(MaxCP);
  };

  auto checkAddressAlignmentWithoutTheField = [&](unsigned AddressOffset,
                                                  unsigned Idx) {
    // Check if the current field is suspicious to be a padding:
    // Return true if the next field is NOT aligned without this field and IS
    // aligned with the field. For the last field, check its previous field
    return (
        (Idx < NumElements - 1 && AddressOffset % FieldSizes[Idx + 1] != 0 &&
         (AddressOffset + FieldSizes[Idx]) % FieldSizes[Idx + 1] == 0) ||
        (Idx == NumElements - 1 && AddressOffset % FieldSizes[Idx - 1] != 0 &&
         (AddressOffset + FieldSizes[Idx]) % FieldSizes[Idx - 1] == 0));
  };

  auto checkFieldSmallerThanNeighbors = [&](unsigned Idx) {
    // If the field is not the last field, check if it's smaller than the next
    // field If the field is the last field, check if it's smaller than the
    // previous field.
    return ((Idx < NumElements - 1 && FieldSizes[Idx] <= FieldSizes[Idx + 1]) ||
            (Idx == NumElements - 1 && FieldSizes[Idx] <= FieldSizes[Idx - 1]));
  };

  unsigned AddressOffset = 0;
  unsigned FieldNum = 0;
  // The loop checks if a field i in StructType can be a padding or not
  // If it's a padding, assign FieldDI[i] as nullptr; otherwise, create
  // a FieldDebugInfo storing a possible FieldNum in the definition in
  // source code
  for (unsigned i = 0; i < NumElements; i++) {
    auto *Ty = StructureType->getElementType(i);
    if (Ty->isArrayTy() && checkNoCPRelationWithOtherFields(i) &&
        checkAddressAlignmentWithoutTheField(AddressOffset, i) &&
        checkFieldSmallerThanNeighbors(i)) {
      // Only ArrayTy can be a padding, as well as:
      // (1) the field has no CP relation with all other fields
      // (2) the field that it's possibly padding for is not aligned without the
      // field (3) its size is smaller than the field it's possibly padding for
      FieldDI[i] = nullptr;
    } else {
      // Only ArrayTy can be a padding, other types is safe. Create a
      // new FieldDebugInfo showing the current field maps to FieldNum
      // in the struct definition
      FieldDI[i] = new FieldDebugInfo(FieldNum++);
    }
    AddressOffset += FieldSizes[i];
  }
}

// Functions of FieldReorderAnalyzer
// Constructor of FieldReorderAnalyzer. Need to calculate CP relations
// according to the new reordering algorithm and based on CloseProximityBuilder
// passed in. Constructor also selects starting fields to put in to the NewOrder
// that is going to be a recommended ordering. Other variables are initialized
// in the base class constructor
FieldReorderAnalyzer::FieldReorderAnalyzer(const Module &CM,
                                           const StructType *ST,
                                           const CloseProximityBuilder *CPB,
                                           const DICompositeType *DI)
    : StructTransformAnalyzer(CM, ST, CPB, DI) {
  DEBUG_WITH_TYPE(DEBUG_TYPE_REORDER,
                  dbgs() << "Initializing FieldReorderAnalyzer of size: "
                         << NumElements << "\n");
  // Calculate every CP relations and save it to an array to avoid repeated
  // calculation
  CloseProximityRelations.resize(NumElements);
  double MaxWCP = 0;
  FieldNumType MaxI, MaxJ;
  for (unsigned i = 0; i < NumElements; i++) {
    CloseProximityRelations[i].resize(NumElements);
    for (unsigned j = 0; j < NumElements; j++) {
      CloseProximityRelations[i][j] = calculateCloseProximity(i, j);
      if (i < j && CloseProximityRelations[i][j] > MaxWCP) {
        MaxWCP = CloseProximityRelations[i][j];
        MaxI = i;
        MaxJ = j;
      }
    }
  }
  if (isCloseToZero(MaxWCP)) {
    Eligibility = false;
    return;
  }
  DEBUG(dbgs() << "CPG for field reordering recommendation\n");
  for (unsigned i = 0; i < NumElements; i++) {
    DEBUG(dbgs() << "F" << i + 1 << "\t");
    for (unsigned j = 0; j < NumElements; j++)
      DEBUG(dbgs() << DEBUG_PRINT_COUNT(CloseProximityRelations[i][j]) << "\t");
    DEBUG(dbgs() << "\n");
  }

  // FIXME: Need to reconsider if the order of the pair makes differences
  NewOrder.push_back(MaxI);
  NewOrder.push_back(MaxJ);
  FieldsToTransform.erase(MaxI);
  FieldsToTransform.erase(MaxJ);
  DEBUG_WITH_TYPE(DEBUG_TYPE_REORDER, dbgs() << "Use F" << MaxI + 1 << " and F"
                                             << MaxJ + 1
                                             << " as seeds in reordering\n");
  assert(MaxWCP == getWCP());
  DEBUG_WITH_TYPE(DEBUG_TYPE_REORDER,
                  dbgs() << "Current WCP: " << DEBUG_PRINT_COUNT(MaxWCP)
                         << "\n");
}

// Calculate Close Proximity in this reordering algorithm. This algorithm
// takes in a pair of CP relation of (count, distance) and return a single
// floating point number that is count/distance. Special cases is that when
// two fields are the same, or they can't fit into the same cache block,
// return 0. When distance is smaller than 1 byte, return the count as if
// the distance is 1 byte.
double
FieldReorderAnalyzer::calculateCloseProximity(FieldNumType Field1,
                                              FieldNumType Field2) const {
  if (Field1 == Field2)
    return 0;
  auto *Pair = CPBuilder->getCloseProximityPair(Field1, Field2);
  if (isCloseToZero(Pair->first))
    return 0;
  if (Pair->second < 1)
    return Pair->first;
  return Pair->first / Pair->second;
}

// Calculate weighted CP (WCP) value of all fields within a cache block
// The function takes an array of fields that can fit on this cache block and
// returns a sum of CP value between every pair of fields on the block
double FieldReorderAnalyzer::calculateWCPWithinCacheBlock(
    std::vector<FieldNumType> *CacheBlock) const {
  double WCP = 0;
  DEBUG_WITH_TYPE(DEBUG_TYPE_REORDER,
                  dbgs() << "Calculate WCP within a cache block: ");
  for (unsigned i = 0; i < CacheBlock->size(); i++) {
    DEBUG_WITH_TYPE(DEBUG_TYPE_REORDER,
                    dbgs() << "F" << (*CacheBlock)[i] + 1 << " ");
    for (unsigned j = i + 1; j < CacheBlock->size(); j++)
      if ((*CacheBlock)[i] < (*CacheBlock)[j])
        WCP += CloseProximityRelations[(*CacheBlock)[i]][(*CacheBlock)[j]];
      else
        WCP += CloseProximityRelations[(*CacheBlock)[j]][(*CacheBlock)[i]];
  }
  DEBUG_WITH_TYPE(DEBUG_TYPE_REORDER,
                  dbgs() << "produces WCP: " << DEBUG_PRINT_COUNT(WCP) << "\n");
  return WCP;
}

// Calculate a total WCP of all fields that are already in NewOrder. Return
// the total WCP. This function is used to measure if the current NewOrder
// is a good ordering
double FieldReorderAnalyzer::getWCP() const {
  unsigned CurrentCacheBlockSize = 0;
  std::vector<FieldNumType> CurrentCacheBlock;
  double WCP = 0;
  for (auto &FieldNum : NewOrder) {
    auto Size = FieldSizes[FieldNum];
    DEBUG_WITH_TYPE(DEBUG_TYPE_REORDER, dbgs() << "Field " << FieldNum + 1
                                               << " size is " << Size << "\n");
    if (!StructureType->getElementType(FieldNum)->isAggregateType() &&
        CurrentCacheBlockSize % Size) {
      // Alignment needed, need to add padding. Ignore alignment for aggregated
      // type (array and struct)
      DEBUG_WITH_TYPE(DEBUG_TYPE_REORDER,
                      dbgs() << "Need to add "
                             << (CurrentCacheBlockSize / Size + 1) * Size -
                                    CurrentCacheBlockSize
                             << " Bytes of padding\n");
      CurrentCacheBlockSize = (CurrentCacheBlockSize / Size + 1) * Size;
      assert(CurrentCacheBlockSize <= CacheBlockSize);
    }
    if (CurrentCacheBlockSize == CacheBlockSize) {
      DEBUG_WITH_TYPE(
          DEBUG_TYPE_REORDER,
          dbgs()
              << "Need to put into a new cache block calculate current one\n");
      WCP += calculateWCPWithinCacheBlock(&CurrentCacheBlock);
      CurrentCacheBlockSize = 0;
      CurrentCacheBlock.clear();
    } else {
      CurrentCacheBlockSize += Size;
      CurrentCacheBlock.push_back(FieldNum);
      if (CurrentCacheBlockSize >= CacheBlockSize) {
        DEBUG_WITH_TYPE(
            DEBUG_TYPE_REORDER,
            dbgs() << "Current cache block cannot fit remaining field\n");
        WCP += calculateWCPWithinCacheBlock(&CurrentCacheBlock);
        CurrentCacheBlockSize %= CacheBlockSize;
        CurrentCacheBlock.clear();
        if (CurrentCacheBlockSize > 0)
          CurrentCacheBlock.push_back(FieldNum);
      }
    }
  }
  WCP += calculateWCPWithinCacheBlock(&CurrentCacheBlock);
  return WCP;
}

// Wrapper function to calculate potential WCP if put field FieldNum at
// the back of NewOrder list
double FieldReorderAnalyzer::estimateWCPAtBack(FieldNumType FieldNum) {
  NewOrder.push_back(FieldNum);
  auto WCP = getWCP();
  NewOrder.pop_back();
  return WCP;
}

// Wrapper function to calculate potential WCP if put field FieldNum at
// the front of NewOrder list
double FieldReorderAnalyzer::estimateWCPAtFront(FieldNumType FieldNum) {
  NewOrder.push_front(FieldNum);
  auto WCP = getWCP();
  NewOrder.pop_front();
  return WCP;
}

// Main function to make suggestions.
// The algorithm is to find a pair of fields that has the largest CP value of
// all fields and put them into NewOrder as seeds (already done in constructor)
// Then try to insert all fields to either front or back of the list and see
// which one can generate the best score (WCP) improvement. Repeat this
// procedure until NewOrder list is established.
void FieldReorderAnalyzer::makeSuggestions() {
  if (!Eligibility) {
    outs() << "Struct has too few fields or cold in profiling for reordering\n";
    return;
  }

  double MaxWCP = getWCP();
  while (FieldsToTransform.size()) {
    FieldNumType MaxField;
    bool InFront = false;
    MaxWCP = 0;
    for (auto &F : FieldsToTransform) {
      // Check if it's better to insert F in the back
      auto WCP = estimateWCPAtBack(F);
      if (WCP > MaxWCP) {
        DEBUG_WITH_TYPE(DEBUG_TYPE_REORDER,
                        dbgs() << "Found better ordering by add F" << F + 1
                               << " in the end\n");
        DEBUG_WITH_TYPE(DEBUG_TYPE_REORDER,
                        dbgs() << "Current WCP: " << DEBUG_PRINT_COUNT(WCP)
                               << "\n");
        MaxWCP = WCP;
        MaxField = F;
        InFront = false;
      }
      // Check if it's better to insert F in the front
      WCP = estimateWCPAtFront(F);
      if (WCP > MaxWCP) {
        DEBUG_WITH_TYPE(DEBUG_TYPE_REORDER,
                        dbgs() << "Found better ordering by add F" << F + 1
                               << " in the front\n");
        DEBUG_WITH_TYPE(DEBUG_TYPE_REORDER,
                        dbgs() << "Current WCP: " << DEBUG_PRINT_COUNT(WCP)
                               << "\n");
        MaxWCP = WCP;
        MaxField = F;
        InFront = true;
      }
    }
    if (InFront)
      NewOrder.push_front(MaxField);
    else
      NewOrder.push_back(MaxField);
    FieldsToTransform.erase(MaxField);
  }

  // Save the calculate order to BestOrder
  std::list<FieldNumType> BestOrder(NewOrder);
  // Create a new NewOrder with original field ordering to compare scores
  NewOrder.clear();
  for (unsigned i = 0; i < NumElements; i++) {
    if (FieldDI[i])
      NewOrder.push_back(i);
  }

  // Calculate the overall score of old ordering. Used to compare if it's worth
  // to reorder fields
  auto OldWCP = getWCP();
  DEBUG_WITH_TYPE(DEBUG_TYPE_REORDER,
                  dbgs() << "New WCP: " << DEBUG_PRINT_COUNT(MaxWCP)
                         << " vs Old WCP: " << DEBUG_PRINT_COUNT(OldWCP)
                         << "\n");
  auto Confidence = 100.0 * (MaxWCP - OldWCP) / OldWCP;
  if (Confidence > 100.0)
    Confidence = 100.0;

  // Print suggestions
  outs() << "Struct definition: " << *StructureType << "\n";
  if (Confidence < 1.0) {
    outs() << "Suggest to keep the current order: \n";
    for (auto &FieldNum : NewOrder) {
      assert(FieldDI[FieldNum]);
      outs() << "F" << FieldDI[FieldNum]->FieldNum + 1 << " ";
    }
    outs() << "\n";
  } else {
    outs() << "Suggest to reorder the fields according to: ";
    for (auto &FieldNum : BestOrder) {
      assert(FieldDI[FieldNum]);
      outs() << "F" << FieldDI[FieldNum]->FieldNum + 1 << "("
             << *StructureType->getElementType(FieldNum) << ")"
             << " ";
    }
    outs() << " might improve performance with " << format("%.0f", Confidence)
           << " % confidence\n";
  }
}

// Functions of StructSplitAnalyzer
// Constructor of StructSplitAnalyzer. Also need to calculate CP relations by
// calling its calculateCloseProximity() functions. Other initializations
// are performed by base class
StructSplitAnalyzer::StructSplitAnalyzer(const Module &CM, const StructType *ST,
                                         const CloseProximityBuilder *CPB,
                                         const DICompositeType *DI)
    : StructTransformAnalyzer(CM, ST, CPB, DI),
      Params({StructSplitColdRatio, StructSplitDistanceThreshold,
              StructSplitMaxSize, StructSplitSizePenalty}) {
  DEBUG_WITH_TYPE(DEBUG_TYPE_SPLIT,
                  dbgs() << "Initializing StructSplitAnalyzer of size: "
                         << NumElements << "\n");
  // Calculate every CP relations and save it to an array to avoid repeated
  // calculation
  CloseProximityRelations.resize(NumElements);
  for (unsigned i = 0; i < NumElements; i++) {
    CloseProximityRelations[i].resize(NumElements);
    for (unsigned j = 0; j < NumElements; j++) {
      CloseProximityRelations[i][j] = calculateCloseProximity(i, j);
    }
  }
  for (unsigned i = 0; i < NumElements; i++) {
    DEBUG(dbgs() << "F" << i + 1 << "\t");
    for (unsigned j = 0; j < NumElements; j++)
      DEBUG(dbgs() << DEBUG_PRINT_COUNT(CloseProximityRelations[i][j]) << "\t");
    DEBUG(dbgs() << "\n");
  }

  // Debug print all parameters used in this suggestion
  DEBUG(dbgs() << "Parameters used in struct spliting: \n");
  DEBUG(dbgs() << "COLD_RATIO: " << Params.ColdRatio << "\n");
  DEBUG(dbgs() << "DISTANCE_THRESHOLD: " << Params.DistanceThreshold << "\n");
  DEBUG(dbgs() << "MAX_SIZE: " << Params.MaxSize << "\n");
  DEBUG(dbgs() << "SIZE_PENALTY: " << Params.SizePenalty << "\n");
  DEBUG(dbgs() << "CPG for struct split recommendation\n");

  DEBUG_WITH_TYPE(DEBUG_TYPE_SPLIT, dbgs() << "Finish constructor\n");
}

// Function to calculate CP value based on CloseProximityBuilder passed in
// Return a floating point value equals to a portion of the count part of
// the CP relations. The portion (SizePenalty) is used to penalize two fields
// with different sizes. (Derived from the paper). Special cases are
// when distance of the pair is larger than a threshold (DistanceThreshold),
// the pair is no longer considered "close" and same applies to when two
// fields are too large (larger than MaxSize).
double StructSplitAnalyzer::calculateCloseProximity(FieldNumType Field1,
                                                    FieldNumType Field2) const {
  auto *Pair = CPBuilder->getCloseProximityPair(Field1, Field2);
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

// Function to find maximum CP value of all pairs in remaining fields
// Return a std::pair type of the two fields
FieldPairType StructSplitAnalyzer::findMaxRemainCP() const {
  double MaxCP = 0;
  auto it = FieldsToTransform.begin();
  auto Field1 = *(it++);
  assert(it != FieldsToTransform.end());
  auto Field2 = *it;
  DEBUG_WITH_TYPE(DEBUG_TYPE_SPLIT,
                  dbgs() << "Try to find a pair with maxmium CP\n");
  for (auto &F1 : FieldsToTransform)
    for (auto &F2 : FieldsToTransform)
      if (F1 != F2) {
        if (CloseProximityRelations[F1][F2] > MaxCP) {
          MaxCP = CloseProximityRelations[F1][F2];
          Field1 = F1;
          Field2 = F2;
        }
      }
  DEBUG_WITH_TYPE(DEBUG_TYPE_SPLIT, dbgs() << "The pair with largest CP is: (F"
                                           << Field1 + 1 << ", F" << Field2 + 1
                                           << ")\n");
  return std::make_pair(Field1, Field2);
}

// Main functions to make suggestions on struct splitting
// The algorithm is to greedily find a pair of fields that has the largest
// CP value in remaining fields and group them together in a new sub-struct.
// Then keep trying to find a remaing field that keeps the total size of
// sub-struct smaller than MaxSize while the average CP value of the newly added
// field with existing fields in the sub-struct is larger enough than a
// threshold (in order to make sure fields in a sub-struct are similarly "close"
// to each other). If we can't such a candidate field, it's done on this
// sub-struct and we move on to create another sub-struct. If we can't find
// a pair of fields with non-zero CP value to create sub-struct, meaning all
// remaining fields are "far" to each other, we will create one single
// sub-struct to hold all the remaining fields.
void StructSplitAnalyzer::makeSuggestions() {
  if (!Eligibility) {
    outs() << "Struct has too few fields or cold in profiling for splitting\n";
    return;
  }
  unsigned dynamicThresholdUpdates = 0;
  DEBUG_WITH_TYPE(DEBUG_TYPE_SPLIT,
                  dbgs() << "Making suggestions for struct splitting\n");
  auto BestPair = findMaxRemainCP();
  double Threshold = CloseProximityRelations[BestPair.first][BestPair.second] /
                     Params.ColdRatio;

  while (FieldsToTransform.size()) {
    if (FieldsToTransform.size() == 1) {
      // Only has one field left, break the loop and create a subrecord for it
      DEBUG_WITH_TYPE(DEBUG_TYPE_SPLIT,
                      dbgs() << "Only have one remaining field...\n");
      break;
    }
    auto BestPair = findMaxRemainCP();
    auto MaxCP = CloseProximityRelations[BestPair.first][BestPair.second];
    if (MaxCP == 0)
      break;
    // If MaxCP is colder than the current threshold, adjust the threhold for
    // several times until the current pair is hotter than threshold or reach
    // the limit of lowering the bar
    while (MaxCP < Threshold) {
      if (dynamicThresholdUpdates++ > Params.MaxNumThresholdUpdates)
        break;
      DEBUG_WITH_TYPE(DEBUG_TYPE_SPLIT,
                      dbgs() << "Lowering threshold by a factor of "
                             << Params.ColdRatio << "\n");
      Threshold /= Params.ColdRatio;
    }
    if (MaxCP < Threshold)
      break;
    DEBUG_WITH_TYPE(DEBUG_TYPE_SPLIT,
                    dbgs() << "CP value is: " << DEBUG_PRINT_COUNT(MaxCP)
                           << " and threshold is: "
                           << DEBUG_PRINT_COUNT(Threshold) << "\n");
    auto *NewRecord = new SubRecordType;
    SubRecords.push_back(NewRecord);
    // Add the pair into the new sub record and remove from remaining fields
    NewRecord->push_back(BestPair.first);
    NewRecord->push_back(BestPair.second);
    DEBUG_WITH_TYPE(DEBUG_TYPE_SPLIT, dbgs() << "Create a new record with F"
                                             << BestPair.first + 1 << " and F"
                                             << BestPair.second + 1 << "\n");
    auto NewRecordSize =
        FieldSizes[BestPair.first] + FieldSizes[BestPair.second];
    FieldsToTransform.erase(BestPair.first);
    FieldsToTransform.erase(BestPair.second);
    // If the pair is enough to take the MAX_SIZE, no need to add more fields to
    // the subrecord
    if (NewRecordSize >= Params.MaxSize)
      continue;
    while (FieldsToTransform.size()) {
      double MaxSum = 0;
      FieldNumType MaxF = 0;
      for (auto &Field : FieldsToTransform) {
        // If the field is too large to fit in the new sub record, give up on
        // this one
        if (FieldSizes[Field] + NewRecordSize > Params.MaxSize)
          continue;
        auto Sum = 0;
        for (auto &Fi : *NewRecord) {
          Sum += CloseProximityRelations[Field][Fi];
        }
        if (Sum > MaxSum) {
          MaxSum = Sum;
          MaxF = Field;
        }
      }
      // Take average
      MaxSum /= NewRecord->size();
      DEBUG_WITH_TYPE(DEBUG_TYPE_SPLIT,
                      dbgs() << "F" << MaxF + 1
                             << " has the best CP relations with new record: "
                             << DEBUG_PRINT_COUNT(MaxSum) << "\n");
      if (MaxSum < Threshold) {
        // None of the remaining fields is suitable to put into this sub record
        break;
      }
      DEBUG_WITH_TYPE(DEBUG_TYPE_SPLIT, dbgs()
                                            << "F" << MaxF + 1
                                            << " is added to the new record\n");
      NewRecord->push_back(MaxF);
      NewRecordSize += FieldSizes[MaxF];
      FieldsToTransform.erase(MaxF);
    }
  }

  // All the remaining fields are cold with others, create one record for all of
  // them
  if (FieldsToTransform.size()) {
    auto *NewRecord = new SubRecordType;
    SubRecords.push_back(NewRecord);
    DEBUG_WITH_TYPE(DEBUG_TYPE_SPLIT, dbgs() << "All remaining fields are "
                                                "cold, create one record for "
                                                "all of them\n");
    for (auto &F : FieldsToTransform) {
      DEBUG_WITH_TYPE(DEBUG_TYPE_SPLIT,
                      dbgs() << "Put F" << F + 1 << " into the cold record\n");
      NewRecord->push_back(F);
    }
    FieldsToTransform.clear();
  }
  assert(FieldsToTransform.size() == 0);

  // Print suggestions
  outs() << "Struct definition: " << *StructureType << "\n";
  for (unsigned i = 0; i < SubRecords.size(); i++) {
    outs() << "Group #" << i + 1 << ":{ ";
    for (auto &F : *SubRecords[i]) {
      outs() << "F" << FieldDI[F]->FieldNum + 1 << "("
             << *StructureType->getElementType(F) << ")"
             << " ";
    }
    outs() << "}\n";
  }
}

// Functions of OldFieldReorderAnalyzer. Here "old" just means the algorithm
// used to reorder fields is derived from prior work in GCC and not necessarily
// mean it's deprecated or worse compared to the other FieldReorderAnalyzer
OldFieldReorderAnalyzer::OldFieldReorderAnalyzer(
    const Module &CM, const StructType *ST, const CloseProximityBuilder *CPB,
    const DICompositeType *DI)
    : FieldReorderAnalyzer(CM, ST, CPB, DI, true),
      DistanceThreshold(StructSplitDistanceThreshold) {
  DEBUG_WITH_TYPE(DEBUG_TYPE_REORDER,
                  dbgs() << "Initializing OldFieldReorderAnalyzer of size: "
                         << NumElements << "\n");
  // Calculate every CP relations and save it to an array to avoid repeated
  // calculation
  CloseProximityRelations.resize(NumElements);
  double MaxWCP = 0;
  for (unsigned i = 0; i < NumElements; i++) {
    CloseProximityRelations[i].resize(NumElements);
    for (unsigned j = 0; j < NumElements; j++) {
      CloseProximityRelations[i][j] = calculateCloseProximity(i, j);
      if (CloseProximityRelations[i][j] > MaxWCP)
        MaxWCP = CloseProximityRelations[i][j];
    }
  }
  if (isCloseToZero(MaxWCP)) {
    Eligibility = false;
    return;
  }
  DEBUG(dbgs() << "CPG for field reordering recommendation\n");
  for (unsigned i = 0; i < NumElements; i++) {
    DEBUG(dbgs() << "F" << i + 1 << "\t");
    for (unsigned j = 0; j < NumElements; j++)
      DEBUG(dbgs() << DEBUG_PRINT_COUNT(CloseProximityRelations[i][j]) << "\t");
    DEBUG(dbgs() << "\n");
  }
}

// Function to calculate CP values. In this algorithm, it interprets (count,
// distance) as using count as it's CP value, if the distance is within a
// predefined threshold.
double
OldFieldReorderAnalyzer::calculateCloseProximity(FieldNumType Field1,
                                                 FieldNumType Field2) const {
  if (Field1 == Field2)
    // In old implementation, actually it's good to have a self-related field
    // But we ignore this
    return 0;
  auto *Pair = CPBuilder->getCloseProximityPair(Field1, Field2);
  if (isCloseToZero(Pair->first))
    return 0;
  if (Pair->second > DistanceThreshold)
    return 0;
  return Pair->first;
}

// Function to calculate FanOut of a field. Here, FanOut means the sum of
// CP values that this field can generate with all the other remaining fields.
// It's used to measure a potential benefit of bringing this field into NewOrder
// and it can provide more opportunites to bring other fields.
double OldFieldReorderAnalyzer::calculateFanOut(FieldNumType FieldNum) const {
  double CPSum = 0;
  for (auto &F : FieldsToTransform) {
    if (F != FieldNum) {
      CPSum += CloseProximityRelations[FieldNum][F];
    }
  }
  return CPSum;
}

// Function to find maximum FanOut among remaining fields
FieldNumType OldFieldReorderAnalyzer::findMaxFanOut() const {
  assert(FieldsToTransform.size());
  double MaxFanOut = 0;
  FieldNumType MaxI = *FieldsToTransform.begin();
  for (auto &F : FieldsToTransform) {
    auto FanOut = calculateFanOut(F);
    if (FanOut > MaxFanOut) {
      MaxFanOut = FanOut;
      MaxI = F;
    }
  }
  DEBUG_WITH_TYPE(DEBUG_TYPE_REORDER, dbgs() << "Current Max FanOut: "
                                             << DEBUG_PRINT_COUNT(MaxFanOut)
                                             << "\n");
  return MaxI;
}

// Function to estimate a potential score (WCP) if put the field at back
// of NewOrder. Return a WCP value
double OldFieldReorderAnalyzer::estimateWCPAtBack(FieldNumType FieldNum) {
  // FIXME: This code is inefficient because of reverse() is called twice on the
  // list It's written like this to reuse the other function but can write
  // better code with higher code efficiency later
  NewOrder.reverse();
  auto WCP = estimateWCPAtFront(FieldNum);
  NewOrder.reverse();
  return WCP;
}

// Function to estimate a potential score (WCP) if put the field at front
// of NewOrder. Return a WCP value
double OldFieldReorderAnalyzer::estimateWCPAtFront(FieldNumType FieldNum) {
  double WCP = 0;
  unsigned TotalSizes = 1;
  for (auto &F : NewOrder) {
    TotalSizes *= FieldSizes[F];
    WCP += CloseProximityRelations[FieldNum][F] / TotalSizes;
  }
  return WCP;
}

// Main function to make suggestions on field reordering.
// The algorithm is to find a field that has the largest FanOut in the remaining
// fields and put it in the NewOrder. Then iterate through all other fields and
// see if putting it in front or back of NewOrder can generate largest WCP. If
// the largest WCP of all remaining fields are zero, find a field with largest
// FanOut again and put it in the front the order. (Put it in the back should
// also be OK. Here we just follow the old GCC implementation). Repeat this
// process until all fields are put into the NewOrder.
void OldFieldReorderAnalyzer::makeSuggestions() {
  if (!Eligibility) {
    outs() << "Struct has too few fields or cold in profiling for reordering\n";
    return;
  }
  auto BestField = findMaxFanOut();
  NewOrder.push_back(BestField);
  FieldsToTransform.erase(BestField);
  DEBUG_WITH_TYPE(DEBUG_TYPE_REORDER, dbgs() << "Use F" << BestField + 1
                                             << " as the seed in reordering\n");

  while (FieldsToTransform.size()) {
    double MaxWCP = 0;
    FieldNumType MaxField;
    bool InFront = false;
    for (auto &F : FieldsToTransform) {
      auto WCP = estimateWCPAtBack(F);
      if (WCP > MaxWCP) {
        MaxWCP = WCP;
        MaxField = F;
        InFront = false;
      }
      WCP = estimateWCPAtFront(F);
      if (WCP > MaxWCP) {
        MaxWCP = WCP;
        MaxField = F;
        InFront = true;
      }
    }

    if (isCloseToZero(MaxWCP)) {
      DEBUG_WITH_TYPE(DEBUG_TYPE_REORDER,
                      dbgs() << "No Field contributes to existing order\n");
      // If none of the remaining field can contribute to existing order, start
      // with maximum fanout again
      MaxField = findMaxFanOut();
      InFront = true; // Either side is fine, old code always put on front
      DEBUG_WITH_TYPE(DEBUG_TYPE_REORDER,
                      dbgs() << "Use F" << MaxField + 1
                             << " as the seed in reordering\n");
    }

    if (InFront) {
      DEBUG_WITH_TYPE(DEBUG_TYPE_REORDER,
                      dbgs() << "Insert F" << MaxField + 1 << " to front\n");
      NewOrder.push_front(MaxField);
    } else {
      DEBUG_WITH_TYPE(DEBUG_TYPE_REORDER,
                      dbgs() << "Insert F" << MaxField + 1 << " to back\n");
      NewOrder.push_back(MaxField);
    }
    FieldsToTransform.erase(MaxField);
  }

  // Print suggestions
  outs() << "Struct definition: " << *StructureType << "\n";
  outs() << "Suggest to reorder the fields according to: ";
  for (auto &FieldNum : NewOrder) {
    assert(FieldDI[FieldNum]);
    outs() << "F" << FieldDI[FieldNum]->FieldNum + 1 << "("
           << *StructureType->getElementType(FieldNum) << ")"
           << " ";
  }
  outs() << "\n";
}
