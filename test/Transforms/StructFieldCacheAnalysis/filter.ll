; Test if ineligible structs can be filtered out and if HotnessAnalyzer works correctly
; The test program defined 5 different struct types.
; 1st is passed to an external function and should be filtered out to guarantee correctness
; 2nd - 4th struct is accessed different number of times and used to test histogram and filter
; according to hotness
; 5th struct is never accessed in the program and should be filtered out
;
; RUN: llvm-as < %s > %t1
; RUN: llvm-lto -O0 -struct-field-cache-analysis -struct-analysis-hotness-cutoff=50 -struct-analysis-number-buckets=3 -debug-only=struct-analysis-detailed-stats -o %t2 %t1 2>&1 | FileCheck %s
; CHECK: Distribution of struct hotness:
; CHECK: Hotness >=0: 1
; CHECK: Hotness >=33: 0
; CHECK: Hotness >=67: 2
; CHECK: There are 2 struct types are accessed in the program
; CHECK: Struct [struct.FooBar4] defined as global struct has 1 accesses and 69 execution count.
; CHECK: Struct [struct.FooBar2] defined as global struct has 1 accesses and 100 execution count.
; CHECK: Case Struct filtered out due to colder than a ratio of maximum hotness was found 1 times


%struct.FooBar1 = type { i32 }
%struct.FooBar2 = type { float }
%struct.FooBar3 = type { double }
%struct.FooBar4 = type { i8 }
%struct.FooBar5 = type { i16 }

@global_foo1 = global %struct.FooBar1 zeroinitializer, align 4
@global_foo2 = global %struct.FooBar2 zeroinitializer, align 4
@global_foo3 = global %struct.FooBar3 zeroinitializer, align 8
@global_foo4 = global %struct.FooBar4 zeroinitializer, align 1
@global_foo5 = global %struct.FooBar5 zeroinitializer, align 2

; Function Attrs: inlinehint noinline norecurse nounwind uwtable
define i32 @main(i32 %argc, i8** %argv) #0 !prof !28 {
entry:
  %retval = alloca i32, align 4
  %argc.addr = alloca i32, align 4
  %argv.addr = alloca i8**, align 8
  %i = alloca i32, align 4
  %tmp = alloca float, align 4
  %i2 = alloca i32, align 4
  %tmp6 = alloca double, align 8
  %i11 = alloca i32, align 4
  %tmp15 = alloca i8, align 1
  store i32 0, i32* %retval, align 4
  store i32 %argc, i32* %argc.addr, align 4
  store i8** %argv, i8*** %argv.addr, align 8
  call void @_Z8ext_funcP7FooBar1(%struct.FooBar1* @global_foo1)
  store i32 0, i32* %i, align 4
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %0 = load i32, i32* %i, align 4
  %cmp = icmp ult i32 %0, 100
  br i1 %cmp, label %for.body, label %for.end, !prof !29

for.body:                                         ; preds = %for.cond
  store float 6.500000e+01, float* getelementptr inbounds (%struct.FooBar2, %struct.FooBar2* @global_foo2, i32 0, i32 0), align 4
  store float 6.500000e+01, float* %tmp, align 4
  %1 = load float, float* %tmp, align 4
  br label %for.inc

for.inc:                                          ; preds = %for.body
  %2 = load i32, i32* %i, align 4
  %inc = add i32 %2, 1
  store i32 %inc, i32* %i, align 4
  br label %for.cond

for.end:                                          ; preds = %for.cond
  store i32 0, i32* %i2, align 4
  br label %for.cond3

for.cond3:                                        ; preds = %for.inc7, %for.end
  %3 = load i32, i32* %i2, align 4
  %cmp4 = icmp ult i32 %3, 10
  br i1 %cmp4, label %for.body5, label %for.end9, !prof !30

for.body5:                                        ; preds = %for.cond3
  store double 6.500000e+01, double* getelementptr inbounds (%struct.FooBar3, %struct.FooBar3* @global_foo3, i32 0, i32 0), align 8
  store double 6.500000e+01, double* %tmp6, align 8
  %4 = load double, double* %tmp6, align 8
  br label %for.inc7

for.inc7:                                         ; preds = %for.body5
  %5 = load i32, i32* %i2, align 4
  %inc8 = add i32 %5, 1
  store i32 %inc8, i32* %i2, align 4
  br label %for.cond3

for.end9:                                         ; preds = %for.cond3
  store i32 0, i32* %i11, align 4
  br label %for.cond12

for.cond12:                                       ; preds = %for.inc16, %for.end9
  %6 = load i32, i32* %i11, align 4
  %cmp13 = icmp ult i32 %6, 70
  br i1 %cmp13, label %for.body14, label %for.end18, !prof !31

for.body14:                                       ; preds = %for.cond12
  store i8 65, i8* getelementptr inbounds (%struct.FooBar4, %struct.FooBar4* @global_foo4, i32 0, i32 0), align 1
  store i8 65, i8* %tmp15, align 1
  %7 = load i8, i8* %tmp15, align 1
  br label %for.inc16

for.inc16:                                        ; preds = %for.body14
  %8 = load i32, i32* %i11, align 4
  %inc17 = add i32 %8, 1
  store i32 %inc17, i32* %i11, align 4
  br label %for.cond12

for.end18:                                        ; preds = %for.cond12
  ret i32 0
}

declare void @_Z8ext_funcP7FooBar1(%struct.FooBar1*) #1

!llvm.ident = !{!0}
!llvm.module.flags = !{!1}

!0 = !{!"clang version 5.0.0 (git@github.com:cmtice/clang-experimental.git 3c6e6f19373f1bd0a6246346730fb82edbaa1c4a) (git@github.com:cmtice/llvm-experimental.git 68fd8a2f7d84272f67f2614bb3da3583af72ff4b)"}
!1 = !{i32 1, !"ProfileSummary", !2}
!2 = !{!3, !4, !5, !6, !7, !8, !9, !10}
!3 = !{!"ProfileFormat", !"InstrProf"}
!4 = !{!"TotalCount", i64 181}
!5 = !{!"MaxCount", i64 100}
!6 = !{!"MaxInternalCount", i64 70}
!7 = !{!"MaxFunctionCount", i64 100}
!8 = !{!"NumCounts", i64 4}
!9 = !{!"NumFunctions", i64 1}
!10 = !{!"DetailedSummary", !11}
!11 = !{!12, !13, !14, !15, !16, !17, !17, !18, !18, !19, !20, !21, !22, !23, !24, !25, !26, !27}
!12 = !{i32 10000, i64 100, i32 1}
!13 = !{i32 100000, i64 100, i32 1}
!14 = !{i32 200000, i64 100, i32 1}
!15 = !{i32 300000, i64 100, i32 1}
!16 = !{i32 400000, i64 100, i32 1}
!17 = !{i32 500000, i64 100, i32 1}
!18 = !{i32 600000, i64 70, i32 2}
!19 = !{i32 700000, i64 70, i32 2}
!20 = !{i32 800000, i64 70, i32 2}
!21 = !{i32 900000, i64 70, i32 2}
!22 = !{i32 950000, i64 10, i32 3}
!23 = !{i32 990000, i64 10, i32 3}
!24 = !{i32 999000, i64 10, i32 3}
!25 = !{i32 999900, i64 10, i32 3}
!26 = !{i32 999990, i64 10, i32 3}
!27 = !{i32 999999, i64 10, i32 3}
!28 = !{!"function_entry_count", i64 1}
!29 = !{!"branch_weights", i32 100, i32 1}
!30 = !{!"branch_weights", i32 10, i32 1}
!31 = !{!"branch_weights", i32 70, i32 1}
