// lib/Tranforms/IPO/StructFieldAccessInfo.cpp - Implements class of
// StructFieldAccessInfo -*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===------------------------------------------------------------------------===//
//
// This file implements class StructFieldAccessInfo and class HotnessAnalyzer
//
// Class StructFieldAccessInfo organizes all memory accesses or function calls
// on each field of this struct type in the whole program. It provides
// interfaces to StructFieldAccessManager to insert new field accesses info and
// provides interfaces to StructFieldAccessManager and CloseProximityBuilder to
// obtain which instruction accesses which field.
//
// Class HotnessAnalyzer provides interfaces for StructFieldAccessManager to
// apply specific filters on all structs in the program according to hotness to
// narrow down the analyze scope or get statistics.
//
//===------------------------------------------------------------------------===//

#include "StructFieldCacheAnalysisImpl.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Operator.h"

using namespace llvm;

static cl::opt<unsigned>
    HistogramSizeForStats("struct-analysis-number-buckets", cl::init(10),
                          cl::Hidden,
                          cl::desc("Number of buckets used in the histogram "
                                   "for analyzing struct hotness"));

static cl::opt<bool> FilterOutZeroHotness(
    "struct-analysis-filter-zero-hotness", cl::init(false), cl::Hidden,
    cl::desc(
        "If true, do not include structs with zero hotness for suggestions"));

static cl::opt<unsigned> HotnessCutoffRatio(
    "struct-analysis-hotness-cutoff", cl::init(0), cl::Hidden,
    cl::desc("Filter out structs that is colder than a ratio of maximum "
             "hotness, should be a percentage."));

static cl::opt<bool> EnableOneFieldMaxPerFunction(
    "struct-analysis-enable-one-field", cl::init(true), cl::Hidden,
    cl::desc("If enabled, only one field can be passed to a function "
             "Otherwise, the function is ineligible to analyze."));

// Functions for StructFieldAccessInfo
// Add a record of load/store Instruction I accessing field FieldNum in the map
void StructFieldAccessInfo::addFieldAccessNum(const Instruction *I,
                                              FieldNumType FieldNum) {
  assert(isa<LoadInst>(I) || isa<StoreInst>(I)); // Only loads and stores
  assert(LoadStoreFieldAccessMap.find(I) == LoadStoreFieldAccessMap.end());
  LoadStoreFieldAccessMap[I] = FieldNum;
  FunctionsToAnalyze.insert(I->getParent()->getParent());
}

// Add a record of call/invoke Instruction I using field FieldNum as its
// argument Arg F is the defition of the callee function
void StructFieldAccessInfo::addFieldAccessNum(const Instruction *I,
                                              const Function *F, unsigned Arg,
                                              FieldNumType FieldNum) {
  assert(isa<CallInst>(I) || isa<InvokeInst>(I)); // Only calls and invokes
  if (F->isDeclaration())
    // Give up if the function only has declaration
    return;
  // Create a new record FunctionCallInfo if this Arg->FieldNum pair of the
  // Instruction is the first pair. Otherwise, insert a new pair of
  // Arg->FieldNum to an existing FunctionCallInfo object
  if (CallInstFieldAccessMap.find(I) == CallInstFieldAccessMap.end()) {
    auto Hotness = getExecutionCount(I);
    if (Hotness.hasValue())
      CallInstFieldAccessMap[I] =
          new FunctionCallInfo(F, Arg, FieldNum, Hotness.getValue());
  } else {
    auto *CallInfo = CallInstFieldAccessMap[I];
    assert(CallInfo->FunctionDeclaration == F);
    auto Hotness = getExecutionCount(I);
    if (Hotness.hasValue())
      CallInfo->insertArgFieldMapping(Arg, FieldNum, Hotness.getValue());
  }
  FunctionsToAnalyze.insert(F);
}

// Function to obtain if the argument is mapped to a field uniquely.
// Return the field number if the argument has a mapping to a field and
// the hotness is non-zero. Otherwise, return None type of Optional
Optional<FieldNumType> StructFieldAccessInfo::getUniqueArgFieldMapping(
    FunctionCallInfoSummary *FuncSummary, ArgNumType ArgNum) const {
  if (FuncSummary->AllArgFieldMappings.size() > 1)
    return None;
  auto *ArgFieldMapping = FuncSummary->AllArgFieldMappings[0];
  assert(ArgNum < ArgFieldMapping->size());
  auto &FieldAccessPair = (*ArgFieldMapping)[ArgNum];
  // Only check the hotness info if FieldNum of this ArgNum is none-zero
  if (FieldAccessPair.first > 0 && FieldAccessPair.second > 0)
    return FieldAccessPair.first;
  return None;
}

// Function to check if the instruction is an access to any field or
// an argument of the function. If it accesses a field number, i.e. it's
// stored in LoadStoreFieldAccessMap, the field number will be written
// onto the reference of AccessedFieldNum as return value. Otherwise,
// if it access a function argument and it has a unique mapping to a
// field number, return the field number. Otherwise, return the argument
// number by assigning it to the reference of AccessedArgNum
void StructFieldAccessInfo::getAccessFieldNumOrArgNum(
    const Instruction *I, Optional<FieldNumType> &AccessedFieldNum,
    Optional<ArgNumType> &AccessedArgNum) const {
  // Check if I directly access loads/stores
  auto FieldAccessIter = LoadStoreFieldAccessMap.find(I);
  if (FieldAccessIter != LoadStoreFieldAccessMap.end()) {
    AccessedFieldNum = FieldAccessIter->second;
    return;
  }
  // Check if I accesses a function argument that can be a field address
  auto ArgNum = StructManager->getLoadStoreArgAccess(I);
  if (ArgNum.hasValue()) {
    // If I is accessing a function argument, check if the arg can be a field
    // address
    auto FuncIter = FunctionCallInfoMap.find(I->getParent()->getParent());
    if (FuncIter == FunctionCallInfoMap.end())
      return;
    if (auto FieldNum =
            getUniqueArgFieldMapping(FuncIter->second, ArgNum.getValue())) {
      // If there's only one field can be the argument, return the FieldNum
      // and will be treated as a normal field
      AccessedFieldNum = FieldNum;
      return;
    } else {
      AccessedArgNum = ArgNum.getValue();
    }
  }
}

// Given a GEP instruction/operator/constant expr, calculate which field address
// it calculates. Return the field number.
FieldNumType
StructFieldAccessInfo::calculateFieldNumFromGEP(const User *U) const {
  DEBUG_WITH_TYPE(DEBUG_TYPE_IR, dbgs() << "Calculating field number from GEP: "
                                        << *U << "\n");
  // Operand 0 should be a pointer to the struct
  auto *Op = U->getOperand(0);
  // Make sure Operand 0 is a struct type and matches the current struct type of
  // StructFieldAccessInfo
  if (Op->getType()->getPointerElementType()->isStructTy() &&
      Op->getType()->getPointerElementType() == StructureType) {
    if (U->getNumOperands() <
        3) // GEP to calculate struct field needs at least 2
           // indices (operand 1 and 2)
      return 0;
    // Operand 1 should be first index to the struct, usually 0; if not 0, it's
    // like goto an element of an array of structs
    Op = U->getOperand(1);
    // TODO: ignore this index for now because it's the same for an array of
    // structs
    assert(Op->getType()->isIntegerTy());
    // Operand 2 should be the index to the field, and be a constant
    Op = U->getOperand(2);
    assert(isa<Constant>(Op));
    auto *Index = cast<Constant>(Op);
    auto Offset = (FieldNumType)Index->getUniqueInteger().getZExtValue();
    assert(Offset < NumElements);
    // TODO: ignore indices after this one. If there's indices, the field has to
    // be an array or struct
    return Offset + 1; // return field number starting from 1
  } else if (Op->getType()->getPointerElementType()->isArrayTy() &&
             Op->getType()
                 ->getPointerElementType()
                 ->getArrayElementType()
                 ->isStructTy() &&
             Op->getType()->getPointerElementType()->getArrayElementType() ==
                 StructureType) {
    if (U->getNumOperands() <
        4) // GEP to calculate struct field needs at least 3
           // indices (operand 1, 2, 3)
      return 0;
    // Operand 1 should be first index to the struct, usually 0; if not 0, it's
    // like goto an element of an array of structs
    Op = U->getOperand(1);
    assert(Op->getType()->isIntegerTy());
    // Operand 2 should be the index to the array of struct
    Op = U->getOperand(2);
    assert(Op->getType()->isIntegerTy());
    // Operand 3 should be the index to the field and be a contant
    Op = U->getOperand(3);
    assert(isa<Constant>(Op));
    auto *Index = cast<Constant>(Op);
    auto Offset = (FieldNumType)Index->getUniqueInteger().getZExtValue();
    assert(Offset < NumElements);
    return Offset + 1;
  } else {
    errs() << *U << "\n";
    errs() << *Op << " " << *Op->getType() << "\n";
    assert(0 && "GEP instruction to analyze is neither from a struct or an "
                "array of struct");
  }
}

// Given a GEP instruction/operator/contant expr, call
// addFieldAccessFromGEPOrBitcast() to add the load or store users to the
// field access map
void StructFieldAccessInfo::addFieldAccessFromGEP(const User *U) {
  DEBUG_WITH_TYPE(DEBUG_TYPE_IR,
                  dbgs() << "Analyze all users of GEP: " << *U << "\n");
  assert(isa<GetElementPtrInst>(U) || isa<GEPOperator>(U) ||
         (isa<ConstantExpr>(U) &&
          cast<ConstantExpr>(U)->getOpcode() == Instruction::GetElementPtr));
  // Have to use getOpcode to check
  // opcode of GetElementPtrConstantExpr because it's private to lib/IR
  auto FieldLoc = calculateFieldNumFromGEP(U);
  if (FieldLoc == 0)
    return;
  addFieldAccessFromGEPOrBitcast(U, FieldLoc);
}

// Core function to find load/store users from a GEP instruction/operator
// or a bitcast instruction/operator.
void StructFieldAccessInfo::addFieldAccessFromGEPOrBitcast(
    const User *U, FieldNumType FieldLoc) {
  for (auto *User : U->users()) {
    DEBUG_WITH_TYPE(DEBUG_TYPE_IR,
                    dbgs() << "Check user of " << *U << ": " << *User << "\n");
    if (isa<Instruction>(User)) {
      auto *Inst = cast<Instruction>(User);
      if (isa<LoadInst>(Inst))
        addFieldAccessNum(Inst, FieldLoc);
      else if (isa<StoreInst>(Inst)) {
        if (U == Inst->getOperand(1))
          addFieldAccessNum(Inst, FieldLoc);
        else
          addStats(StructFieldAccessManager::DebugStats::
                       DS_FieldAddressStoredInAnotherVariable);
      } else {
        if (isa<CallInst>(Inst) || isa<InvokeInst>(Inst)) {
          DEBUG_WITH_TYPE(DEBUG_TYPE_IR, dbgs() << "Found a call inst\n");
          ImmutableCallSite Call(Inst);
          auto *Func = Call.getCalledFunction();
          if (Func && Func->arg_size() == Call.getNumArgOperands()) {
            DEBUG_WITH_TYPE(DEBUG_TYPE_IR,
                            dbgs() << "Call on function: " << *Func << "\n");
            for (unsigned i = 0; i < Call.getNumArgOperands(); i++) {
              if (Call.getArgOperand(i) == U) {
                DEBUG_WITH_TYPE(DEBUG_TYPE_IR,
                                dbgs() << "Found a call/invoke on field F"
                                       << FieldLoc << "\n");
                addFieldAccessNum(Inst, Func, i, FieldLoc);
              }
            }
          } else {
            // Bail out if Gep is used in an indirect function or a function
            // that has undetermined args
            addStats(StructFieldAccessManager::DebugStats::
                         DS_GepPassedIntoIndirectFunc);
          }
        } else if (isa<BitCastInst>(Inst))
          // Recursively call the function itself to peel one level of bitcast
          addFieldAccessFromGEPOrBitcast(Inst, FieldLoc);
        else
          // TODO: Collect stats of this kind of access and add analysis later
          addStats(StructFieldAccessManager::DebugStats::DS_GepUnknownUse,
                   Inst->getOpcode());
      }
    } else if (isa<Operator>(User)) {
      auto *Oper = cast<Operator>(User);
      if (isa<BitCastOperator>(Oper))
        // Recursively call the function itself to peel one level of bitcast
        addFieldAccessFromGEPOrBitcast(Oper, FieldLoc);
      else
        // TODO: Collect stats of this kind of access and add analysis later
        addStats(StructFieldAccessManager::DebugStats::DS_GepUnknownUse,
                 Oper->getOpcode());
    } else
      addStats(StructFieldAccessManager::DebugStats::
                   DS_UserNotInstructionNorOperator);
  }
}

// Main function to analyze field accesses from a struct variable
void StructFieldAccessInfo::analyzeUsersOfStructValue(const Value *V) {
  assert(V->getType()->isPointerTy() &&
         V->getType()->getPointerElementType()->isStructTy());
  for (auto *U : V->users()) {
    DEBUG_WITH_TYPE(DEBUG_TYPE_IR,
                    dbgs() << "Analyzing user of " << *V << ": " << *U << "\n");
    if (isa<Instruction>(U)) {
      auto *Inst = cast<Instruction>(U);
      if (!isa<GetElementPtrInst>(Inst)) {
        // Only support access struct through GEP for now
        if (isa<CallInst>(Inst) || isa<InvokeInst>(Inst)) {
          ImmutableCallSite Call(Inst);
          DEBUG_WITH_TYPE(DEBUG_TYPE_IR, dbgs()
                                             << "User is a call instruction\n");
          auto *F = Call.getCalledFunction();
          if (!F || F->isDeclaration())
            // If a struct is passed to an indirect or a function not declared
            // in the program, we can't analyze it...
            Eligiblity = false;
        }
        continue;
      } else {
        assert(cast<GetElementPtrInst>(Inst)->getPointerOperand() == V);
        if (Inst->getType()->getPointerElementType() ==
            V->getType()->getPointerElementType()) {
          // If an GEP is used to calculate a struct type from a struct type,
          // it should be an array index of a struct array
          analyzeUsersOfStructValue(Inst);
        } else {
          addFieldAccessFromGEP(Inst);
        }
      }
    } else if (isa<Operator>(U)) {
      auto *Oper = cast<Operator>(U);
      if (!isa<GEPOperator>(Oper))
        // Only support access struct through GEP for now
        continue;
      addFieldAccessFromGEP(Oper);
    } else if (isa<ConstantExpr>(U)) {
      auto *Const = cast<ConstantExpr>(U);
      if (Const->getOpcode() != Instruction::GetElementPtr)
        // if (!isa<GetElementPtrConstantExpr>(Const)) {
        continue;
      addFieldAccessFromGEP(Const);
    } else
      addStats(StructFieldAccessManager::DebugStats::
                   DS_UserNotInstructionNorOperator);
  }
}

// Main function to analyze field accesses from a struct pointer value
void StructFieldAccessInfo::analyzeUsersOfStructPointerValue(const Value *V) {
  // Analyze users of value defined as struct*
  assert(V->getType()->isPointerTy() &&
         V->getType()->getPointerElementType()->isPointerTy() &&
         V->getType()
             ->getPointerElementType()
             ->getPointerElementType()
             ->isStructTy());

  for (auto *U : V->users()) {
    DEBUG_WITH_TYPE(DEBUG_TYPE_IR,
                    dbgs() << "Analyzing user of " << *V << ": " << *U << "\n");
    if (isa<Instruction>(U)) {
      auto *Inst = cast<Instruction>(U);
      if (isa<LoadInst>(Inst) &&
          Inst->getType()->getPointerElementType()->isStructTy())
        analyzeUsersOfStructValue(Inst);
      else if (isa<GetElementPtrInst>(Inst))
        addStats(StructFieldAccessManager::DebugStats::DS_GepUsedOnStructPtr);
      else if (!isa<StoreInst>(Inst) && !isa<BitCastInst>(Inst)) {
        addStats(
            StructFieldAccessManager::DebugStats::DS_UnknownUsesOnStructPtr);
      } else if (isa<Operator>(U)) {
        auto *Oper = cast<Operator>(U);
        if (isa<GEPOperator>(Oper))
          addStats(StructFieldAccessManager::DebugStats::DS_GepUsedOnStructPtr);
        else if (!isa<BitCastOperator>(Oper))
          addStats(
              StructFieldAccessManager::DebugStats::DS_UnknownUsesOnStructPtr);
      } else
        addStats(StructFieldAccessManager::DebugStats::
                     DS_UserNotInstructionNorOperator);
    }
  }
}

// Main function to analyze field accesses from a struct array value
void StructFieldAccessInfo::analyzeUsersOfStructArrayValue(const Value *V) {
  // Analyze users of value defined as struct[]
  assert(V->getType()->isPointerTy() &&
         V->getType()->getPointerElementType()->isArrayTy() &&
         V->getType()
             ->getPointerElementType()
             ->getArrayElementType()
             ->isStructTy());
  for (auto *U : V->users()) {
    DEBUG_WITH_TYPE(DEBUG_TYPE_IR,
                    dbgs() << "Analyzing user of " << *V << ": " << *U << "\n");
    if (isa<Instruction>(U)) {
      auto *Inst = cast<Instruction>(U);
      // Only GEP instruction is valid on a struct array
      if (isa<GetElementPtrInst>(Inst)) {
        if (Inst->getType()->isPointerTy() &&
            Inst->getType()->getPointerElementType()->isStructTy()) {
          // If result of the GEP value is the address of a struct, which
          // is the same as the result of an alloc of a struct definition
          if (Inst->getType()->getPointerElementType() == StructureType)
            // Only this kind of struct is allow because sometimes an array
            // can store an address of base class
            analyzeUsersOfStructValue(Inst);
        } else {
          // If the result of GEP is not a struct, then the GEP is directly
          // used to calculate a field of this struct, directly send it to
          // add field access
          addFieldAccessFromGEP(Inst);
        }
      } else if (!isa<BitCastInst>(Inst)) {
        addStats(
            StructFieldAccessManager::DebugStats::DS_UnknownUsesOnStructArray);
      }
    } else if (isa<Operator>(U)) {
      auto *Oper = cast<Operator>(U);
      if (isa<GEPOperator>(Oper)) {
        // Same as GEP instruction above
        if (Oper->getType()->isPointerTy() &&
            Oper->getType()->getPointerElementType()->isStructTy()) {
          analyzeUsersOfStructValue(Oper);
        } else {
          addFieldAccessFromGEP(Oper);
        }
      } else if (!isa<BitCastOperator>(Oper)) {
        addStats(
            StructFieldAccessManager::DebugStats::DS_UnknownUsesOnStructArray);
      }
    } else {
      addStats(StructFieldAccessManager::DebugStats::
                   DS_UserNotInstructionNorOperator);
    }
  }
}

// Functions that iterates through all the Call/Invoke instructions that
// takes field address as an input and summarizes the field address information
// into the function definition
void StructFieldAccessInfo::summarizeFunctionCalls() {
  DEBUG_WITH_TYPE(DEBUG_TYPE_IR, dbgs() << "Summarizes call/invokes "
                                           "instructions into function "
                                           "declarations\n");
  for (auto &it : CallInstFieldAccessMap) {
    auto *CallInfo = it.second;
    assert(CallInfo);
    auto *F = CallInfo->FunctionDeclaration;
    assert(F);
    if (FunctionCallInfoMap.find(F) == FunctionCallInfoMap.end())
      FunctionCallInfoMap[F] =
          new FunctionCallInfoSummary(&CallInfo->ArgFieldMappingArray);
    else
      FunctionCallInfoMap[F]->insertCallInfo(&CallInfo->ArgFieldMappingArray);
  }
  for (auto &it : FunctionCallInfoMap) {
    DEBUG_WITH_TYPE(DEBUG_TYPE_IR,
                    dbgs() << "Function " << it.first->getName()
                           << " is called with fields as argument:\n");
    if (it.second->AllArgFieldMappings.size() > 1)
      addStats(StructFieldAccessManager::DebugStats::
                   DS_DifferentFieldsPassedIntoArg);
    for (auto *Args : it.second->AllArgFieldMappings)
      for (unsigned i = 0; i < Args->size(); i++) {
        auto &FieldAccessPair = (*Args)[i];
        if (FieldAccessPair.first > 0)
          DEBUG_WITH_TYPE(DEBUG_TYPE_IR,
                          dbgs() << "Arg " << i << " is called as field "
                                 << FieldAccessPair.first << " with hotness "
                                 << FieldAccessPair.second << "\n");
      }
  }
}

// Function to calculate the total hotness of all the field accesses of
// the struct
ProfileCountType StructFieldAccessInfo::calculateTotalHotness() const {
  ProfileCountType Hotness = 0;
  auto IncrementHotness = [&](const Instruction *I) {
    auto Count = getExecutionCount(I);
    if (Count.hasValue())
      Hotness += Count.getValue();
  };

  for (auto &it : LoadStoreFieldAccessMap) {
    IncrementHotness(it.first);
  }
  for (auto &it : CallInstFieldAccessMap) {
    IncrementHotness(it.first);
  }
  return Hotness;
}

// (For debug only) Debug print all the field access instructions of
// the struct
void StructFieldAccessInfo::debugPrintAllStructAccesses(raw_ostream &OS) {
  for (auto &it : LoadStoreFieldAccessMap) {
    OS << "\tInstruction [" << *it.first << "] accesses field number ["
       << it.second << "]\n";
  }
  for (auto &it : CallInstFieldAccessMap) {
    OS << "\tInstruction [" << *it.first << "] calls function "
       << it.second->FunctionDeclaration->getName() << " access fields\n";
  }
}

// Functions for StructHotnessAnalyzer
// Function to add a struct type to the hotness map for analysis
void StructHotnessAnalyzer::addStruct(const StructFieldAccessInfo *SI) {
  auto Hotness = SI->calculateTotalHotness();
  if (Hotness > MaxHotness)
    MaxHotness = Hotness;
  StructHotness[SI->getStructType()] = Hotness;
}

// Function to generate a histogram based on the hotness of the structs
// added to the StructHotness map
void StructHotnessAnalyzer::generateHistogram() {
  // TODO: vary the number of buckets
  Histogram.resize(HistogramSizeForStats);
  auto MaxHotnessUpperBound =
      MaxHotness + 1; // To avoid the largest one out of bound
  for (auto &it : StructHotness) {
    auto Index = it.second * HistogramSizeForStats / MaxHotnessUpperBound;
    Histogram[Index]++;
  }
  outs() << "Distribution of struct hotness: \n";
  for (unsigned i = 0; i < Histogram.size(); i++)
    outs() << "Hotness >=" << MaxHotnessUpperBound * i / HistogramSizeForStats
           << ": " << Histogram[i] << "\n";
}

// Function to check if a struct type can be seen as hot or not in the
// current configuration. Return true if it's regarded as hot.
// Hot or not is decided by two configuraiton flags: FilterOutZeroHotness
// and HotnessCutoffRatio, and the hotness of the struct
bool StructHotnessAnalyzer::isHot(const StructFieldAccessInfo *SI) const {
  auto *T = SI->getStructType();
  auto it = StructHotness.find(T);
  assert(it != StructHotness.end());

  if (FilterOutZeroHotness && it->second == 0)
    return false;
  if (HotnessCutoffRatio == 0)
    return true;
  return (it->second > MaxHotness * HotnessCutoffRatio / 100);
}
