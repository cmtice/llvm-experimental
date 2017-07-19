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
#include "llvm/IR/AssemblyAnnotationWriter.h"
#include "llvm/Pass.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/IPO/StructFieldCacheAnalysis.h"

#include <unordered_map>
#include <vector>

using namespace llvm;

namespace llvm {
typedef unsigned FieldNumType;
typedef std::pair<const StructType *, FieldNumType> StructInfoMapPairType;
typedef uint64_t ExecutionCountType;
typedef std::vector<FieldNumType> FieldNumArrayType;

class StructFieldAccessInfo;

/// This class is used to analyze the hotness of each struct
class StructHotnessAnalyzer {
public:
  StructHotnessAnalyzer() : MaxHotness(0) {}
  void addStruct(const StructFieldAccessInfo *SI);
  void generateHistogram();
  bool isHot(const StructFieldAccessInfo *SI) const;

private:
  ExecutionCountType MaxHotness;
  std::unordered_map<const StructType *, ExecutionCountType> StructHotness;
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
                           function_ref<BlockFrequencyInfo *(Function &)> LBFI)
      : CurrentModule(M), LookupBFI(LBFI),
        StatCounts(DebugStats::DS_MaxNumStats) {
    HotnessAnalyzer = new StructHotnessAnalyzer;
  };

  ~StructFieldAccessManager() { delete HotnessAnalyzer; }

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

  /// Retrive a pair of information if the instruction is accessing any struct
  /// type and field number
  Optional<StructInfoMapPairType>
  getFieldAccessOnInstruction(const Instruction *I) const;

  /// Summarizes all CallInst and InvokeInst into function declarations
  void summarizeFunctionCalls();

  /// Apply some filters to reduce the number of struct in analysis
  void applyFiltersToStructs();

  /// Print all accesses of all struct types defined in the program
  void debugPrintAllStructAccesses();

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

  /// A map storing access info of all structs
  std::unordered_map<const StructType *, StructFieldAccessInfo *>
      StructFieldAccessInfoMap;

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
  /// This struct organizes a call on a function with each argument access which
  /// struct field
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
  /// This struct organizes all calls on a function definition with all mappings
  /// of arguments and struct field number
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
  Optional<ExecutionCountType> getExecutionCount(const BasicBlock *BB) const {
    return StructManager->getExecutionCount(BB);
  }

  Optional<ExecutionCountType> getExecutionCount(const Instruction *I) const {
    return StructManager->getExecutionCount(I->getParent());
  }
  /// %}

  /// Iterate through all Call/Invoke instructions that accesses a field and
  /// summarize them into the function definitions
  void summarizeFunctionCalls();

  /// Calculate total hotness of all load/store field accesses
  ExecutionCountType calculateTotalHotness() const;

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
  /// For stats
  std::vector<unsigned> StatCounts;
  std::unordered_map<unsigned, unsigned> UnknownOpcodes;

  /// A map records all load/store instructions accessing which field of the
  /// structure
  std::unordered_map<const Instruction *, unsigned> LoadStoreFieldAccessMap;

  /// A map records all call/invoke instructions accessing which field of the
  /// structure
  std::unordered_map<const Instruction *, FunctionCallInfo *>
      CallInstFieldAccessMap;

  /// A map records all functions that has calls with field accesses and their
  /// calling patterns
  std::unordered_map<const Function *, FunctionAccessPattern *>
      FunctionAccessMap;

private:
  /// Calculate which field of the struct is the GEP pointing to, from
  /// GetElementPtrInst or GEPOperator
  FieldNumType calculateFieldNumFromGEP(const User *U) const;

  /// Record all users of a GEP instruction/operator that calculates the address
  /// of a field.
  void addFieldAccessFromGEP(const User *U);

  /// Record an access pattern in the data structure for a load/store
  void addFieldAccessNum(const Instruction *I, FieldNumType FieldNum);

  /// Record an access pattern in the data structure for a call/invoke
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

  /// Override the base class function to print an annotate message after each
  /// basic block
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

  /// Override the base class function to print an annotate message after each
  /// Instruction
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
} // namespace llvm

#endif
