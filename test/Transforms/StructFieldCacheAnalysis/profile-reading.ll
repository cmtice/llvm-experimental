; Test reading profile data. There is only one function and a loop called 100 times. Inside
; the loop, there are two loads on a struct. Each of them should have 100 execution counts.
;
; RUN: llvm-as < %s > %t1
; RUN: llvm-lto -O0 -struct-field-cache-analysis -o %t2 %t1 | FileCheck %s

; CHECK: There are 1 struct types are accessed in the program
; CHECK: Struct [struct.anon] defined as global struct has 2 accesses and 200 execution count.

%struct.anon = type { i32, double }

@FooBar = internal global %struct.anon zeroinitializer, align 8

; Function Attrs: inlinehint noinline norecurse nounwind uwtable
define i32 @main() #0 !prof !28 {
entry:
  %retval = alloca i32, align 4
  %i = alloca i32, align 4
  store i32 0, i32* %retval, align 4
  store i32 0, i32* %i, align 4
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %0 = load i32, i32* %i, align 4
  %cmp = icmp ult i32 %0, 100
  br i1 %cmp, label %for.body, label %for.end, !prof !29

for.body:                                         ; preds = %for.cond
  store i32 1, i32* getelementptr inbounds (%struct.anon, %struct.anon* @FooBar, i32 0, i32 0), align 8
  store double 59.0, double* getelementptr inbounds (%struct.anon, %struct.anon* @FooBar, i32 0, i32 1), align 8
  br label %for.inc

for.inc:                                          ; preds = %for.body
  %1 = load i32, i32* %i, align 4
  %inc = add i32 %1, 1
  store i32 %inc, i32* %i, align 4
  br label %for.cond

for.end:                                          ; preds = %for.cond
  ret i32 0
}

!llvm.ident = !{!0}
!llvm.module.flags = !{!1}

!0 = !{!"clang version 5.0.0 (git@github.com:cmtice/clang-experimental.git 3c6e6f19373f1bd0a6246346730fb82edbaa1c4a) (git@github.com:cmtice/llvm-experimental.git 68fd8a2f7d84272f67f2614bb3da3583af72ff4b)"}
!1 = !{i32 1, !"ProfileSummary", !2}
!2 = !{!3, !4, !5, !6, !7, !8, !9, !10}
!3 = !{!"ProfileFormat", !"InstrProf"}
!4 = !{!"TotalCount", i64 101}
!5 = !{!"MaxCount", i64 100}
!6 = !{!"MaxInternalCount", i64 1}
!7 = !{!"MaxFunctionCount", i64 100}
!8 = !{!"NumCounts", i64 2}
!9 = !{!"NumFunctions", i64 1}
!10 = !{!"DetailedSummary", !11}
!11 = !{!12, !13, !14, !15, !16, !17, !17, !18, !18, !19, !20, !21, !22, !23, !24, !25, !26, !27}
!12 = !{i32 10000, i64 100, i32 1}
!13 = !{i32 100000, i64 100, i32 1}
!14 = !{i32 200000, i64 100, i32 1}
!15 = !{i32 300000, i64 100, i32 1}
!16 = !{i32 400000, i64 100, i32 1}
!17 = !{i32 500000, i64 100, i32 1}
!18 = !{i32 600000, i64 100, i32 1}
!19 = !{i32 700000, i64 100, i32 1}
!20 = !{i32 800000, i64 100, i32 1}
!21 = !{i32 900000, i64 100, i32 1}
!22 = !{i32 950000, i64 100, i32 1}
!23 = !{i32 990000, i64 100, i32 1}
!24 = !{i32 999000, i64 100, i32 1}
!25 = !{i32 999900, i64 100, i32 1}
!26 = !{i32 999990, i64 100, i32 1}
!27 = !{i32 999999, i64 100, i32 1}
!28 = !{!"function_entry_count", i64 1}
!29 = !{!"branch_weights", i32 100, i32 1}