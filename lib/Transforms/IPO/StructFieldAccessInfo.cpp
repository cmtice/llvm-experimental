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
// This file implements StructFieldAccessInfo that is used in
// StructFieldCacheAnalysis pass.
//
//===------------------------------------------------------------------------===//

#include "StructFieldCacheAnalysisImpl.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Operator.h"

using namespace llvm;

static cl::opt<unsigned> HistogramSizeForStats(
    "struct-analysis-number-buckets", cl::init(10), cl::Hidden,
    cl::desc("Number of buckets used to analyze struct hotness"));

static cl::opt<unsigned> HotnessCutoffRatio(
    "struct-analysis-hotness-cutoff", cl::init(0), cl::Hidden,
    cl::desc("Filter out structs that is colder than a ratio of maximum "
             "hotness, should be a percentage."));

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
    CallInstFieldAccessMap[I] = new FunctionCallInfo(F, Arg, FieldNum);
  } else {
    auto *CallSite = CallInstFieldAccessMap[I];
    assert(CallSite->FunctionDeclaration == F);
    CallSite->insertCallInfo(Arg, FieldNum);
  }
  //FIXME: add the function F to Functions to analyze
}

Optional<FieldNumType>
StructFieldAccessInfo::getAccessFieldNum(const Instruction *I) const {
  Optional<FieldNumType> ret;
  auto it = LoadStoreFieldAccessMap.find(I);
  if (it != LoadStoreFieldAccessMap.end()) {
    ret = it->second;
  }
  return ret;
}

FieldNumType
StructFieldAccessInfo::calculateFieldNumFromGEP(const User *U) const {
  DEBUG_WITH_TYPE(DEBUG_TYPE_IR, dbgs() << "Calculating field number from GEP: "
                                        << *U << "\n");
  // Operand 0 should be a pointer to the struct
  assert(isa<GetElementPtrInst>(U) || isa<GEPOperator>(U) ||
         (isa<ConstantExpr>(U) &&
          cast<ConstantExpr>(U)->getOpcode() == Instruction::GetElementPtr));
  // Have to use getOpcode to check
  // opcode of GetElementPtrConstantExpr because it's private to lib/IR
  auto *Op = U->getOperand(0);
  // Make sure Operand 0 is a struct type and matches the current struct type of
  // StructFieldAccessInfo
  assert(Op->getType()->getPointerElementType()->isStructTy() &&
         Op->getType()->getPointerElementType() == StructureType);
  if (U->getNumOperands() < 3) // GEP to calculate struct field needs at least 2
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
}

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
      } else {
        if (isa<CallInst>(Inst) || isa<InvokeInst>(Inst)) {
          ImmutableCallSite Call(Inst);
          auto *Func = Call.getCalledFunction();
          if (Func && Func->arg_size() == Call.getNumArgOperands()) {
            for (unsigned i = 0; i < Call.getNumArgOperands(); i++) {
              if (Call.getArgOperand(i) == U)
                addFieldAccessNum(Inst, Func, i, FieldLoc);
            }
          } else {
            // Bail out if Gep is used in an indirect function or a function
            // that has undetermined args
            addStats(StructFieldAccessManager::DebugStats::
                         DS_GepPassedIntoIndirectFunc);
          }
        } else if (isa<BitCastInst>(Inst)) {
          addStats(
              StructFieldAccessManager::DebugStats::DS_GepPassedIntoBitcast);
        } else {
          // TODO: Collect stats of this kind of access and add analysis later
          addStats(StructFieldAccessManager::DebugStats::DS_GepUnknownUse,
                   Inst->getOpcode());
        }
      }
    } else if (isa<Operator>(User)) {
      auto *Oper = cast<Operator>(User);
      if (isa<BitCastOperator>(Oper)) {
        addStats(StructFieldAccessManager::DebugStats::DS_GepPassedIntoBitcast);
      } else {
        // TODO: Collect stats of this kind of access and add analysis later
        addStats(StructFieldAccessManager::DebugStats::DS_GepUnknownUse,
                 Oper->getOpcode());
      }
    } else {
      addStats(StructFieldAccessManager::DebugStats::
                   DS_UserNotInstructionNorOperator);
    }
  }
}

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
          if (!F || F->isDeclaration()) {
            // If a struct is passed to an indirect or a function not declared
            // in the program, we can't analyze it...
            Eligiblity = false;
          }
        }
        continue;
      }
      addFieldAccessFromGEP(Inst);
    } else if (isa<Operator>(U)) {
      auto *Oper = cast<Operator>(U);
      if (!isa<GEPOperator>(Oper)) {
        // Only support access struct through GEP for now
        continue;
      }
      addFieldAccessFromGEP(Oper);
    } else if (isa<ConstantExpr>(U)) {
      auto *Const = cast<ConstantExpr>(U);
      if (Const->getOpcode() != Instruction::GetElementPtr) {
        // if (!isa<GetElementPtrConstantExpr>(Const)) {
        continue;
      }
      addFieldAccessFromGEP(Const);
    } else {
      addStats(StructFieldAccessManager::DebugStats::
                   DS_UserNotInstructionNorOperator);
    }
  }
}

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
      if (isa<LoadInst>(Inst)) {
        analyzeUsersOfStructValue(Inst);
      } else if (isa<GetElementPtrInst>(Inst)) {
        addStats(StructFieldAccessManager::DebugStats::DS_GepUsedOnStructPtr);
      } else {
        addStats(
            StructFieldAccessManager::DebugStats::DS_UnknownUsesOnStructPtr);
      }
    } else if (isa<Operator>(U)) {
      auto *Oper = cast<Operator>(U);
      if (isa<GEPOperator>(Oper)) {
        addStats(StructFieldAccessManager::DebugStats::DS_GepUsedOnStructPtr);
      } else {
        addStats(
            StructFieldAccessManager::DebugStats::DS_UnknownUsesOnStructPtr);
      }
    } else {
      addStats(StructFieldAccessManager::DebugStats::
                   DS_UserNotInstructionNorOperator);
    }
  }
}

void StructFieldAccessInfo::summarizeFunctionCalls() {
  DEBUG_WITH_TYPE(DEBUG_TYPE_IR, dbgs() << "Summarizes call/invokes "
                                           "instructions into function "
                                           "declarations\n");
  for (auto &it : CallInstFieldAccessMap) {
    auto *CallSiteInfo = it.second;
    assert(CallSiteInfo);
    auto *F = CallSiteInfo->FunctionDeclaration;
    assert(F);
    if (FunctionAccessMap.find(F) == FunctionAccessMap.end()) {
      FunctionAccessMap[F] =
          new FunctionAccessPattern(&CallSiteInfo->Arguments);
    } else {
      FunctionAccessMap[F]->insertCallInfo(&CallSiteInfo->Arguments);
    }
  }
  for (auto &it : FunctionAccessMap) {
    DEBUG_WITH_TYPE(DEBUG_TYPE_IR,
                    dbgs() << "Function " << it.first->getName()
                           << " is called with fields as argument:\n");
    for (auto *Args : it.second->CallSites) {
      for (unsigned i = 0; i < Args->size(); i++) {
        if ((*Args)[i] > 0)
          DEBUG_WITH_TYPE(DEBUG_TYPE_IR, dbgs() << "Arg " << i
                                                << " is called as field "
                                                << (*Args)[i] << "\n");
      }
    }
  }
}

ExecutionCountType StructFieldAccessInfo::calculateTotalHotness() const {
  ExecutionCountType Hotness = 0;
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
void StructHotnessAnalyzer::addStruct(const StructFieldAccessInfo *SI) {
  auto Hotness = SI->calculateTotalHotness();
  if (Hotness > MaxHotness)
    MaxHotness = Hotness;
  StructHotness[SI->getStructType()] = Hotness;
}

void StructHotnessAnalyzer::generateHistogram() {
  // TODO: vary the number of buckets
  Histogram.resize(HistogramSizeForStats);
  auto MaxHotnessUpperBound =
      MaxHotness + 1; // To avoid the largest one out of bound
  for (auto &it : StructHotness) {
    auto Index = it.second * HistogramSizeForStats / MaxHotnessUpperBound;
    Histogram[Index]++;
  }
  dbgs() << "Distribution of struct hotness: \n";
  for (unsigned i = 0; i < Histogram.size(); i++)
    dbgs() << "Hotness >=" << MaxHotnessUpperBound * i / HistogramSizeForStats
           << ": " << Histogram[i] << "\n";
}

bool StructHotnessAnalyzer::isHot(const StructFieldAccessInfo *SI) const {
  if (HotnessCutoffRatio == 0)
    return true;
  auto *T = SI->getStructType();
  auto it = StructHotness.find(T);
  assert(it != StructHotness.end());
  return (it->second > MaxHotness * HotnessCutoffRatio / 100);
}
