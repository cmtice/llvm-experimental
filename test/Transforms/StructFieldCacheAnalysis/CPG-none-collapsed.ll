; Test if CPG collapsing algorithm is correct
; This test checks when part of CPG is not collapsable, i.e. CPG can't be collapsed to a single node. It still
; uses golden checker to check CPG results. This test only needs to detect if there's error reported.
;
; RUN: llvm-as < %s > %t1
; RUN: llvm-lto -O0 -struct-field-cache-analysis -struct-analysis-check-CPG -o %t2 %t1 2>&1 | FileCheck %s
; CHECK-NOT: Found error in CPG

%struct.FooBar = type { i32, i32, i32, i32 }

@global_foo = global %struct.FooBar zeroinitializer, align 8

; Function Attrs: inlinehint
define i32 @main() #0 !prof !27 {
entry:
  %call = call i64 @time(i64* null)
  %conv = trunc i64 %call to i32
  call void @srand(i32 %conv)
  store i32 10, i32* getelementptr inbounds (%struct.FooBar, %struct.FooBar* @global_foo, i32 0, i32 0), align 8
  br label %BB1

BB1:                                              ; preds = %BB2, %entry
  store i32 20, i32* getelementptr inbounds (%struct.FooBar, %struct.FooBar* @global_foo, i32 0, i32 1), align 8
  %call1 = call i32 @rand()
  %rand1 = srem i32 %call1, 50
  %cmp1 = icmp ult i32 %rand1, 20
  br i1 %cmp1, label %BB2, label %BB3, !prof !28

BB2:                                              ; preds = %BB1, %BB3
  store i32 30, i32* getelementptr inbounds (%struct.FooBar, %struct.FooBar* @global_foo, i32 0, i32 2), align 8
  %call2 = call i32 @rand()
  %rand2 = srem i32 %call2, 50
  %cmp2 = icmp ult i32 %rand2, 25
  br i1 %cmp1, label %BB1, label %BB3, !prof !29

BB3:                                              ; preds = %BB1, %BB2
  store i32 40, i32* getelementptr inbounds (%struct.FooBar, %struct.FooBar* @global_foo, i32 0, i32 3), align 8
  %call3 = call i32 @rand()
  %rand3 = srem i32 %call3, 50
  %cmp3 = icmp ult i32 %rand3, 25
  br i1 %cmp3, label %BB2, label %BB4, !prof !30

BB4:                                              ; preds = %BB3
  store i32 10, i32* getelementptr inbounds (%struct.FooBar, %struct.FooBar* @global_foo, i32 0, i32 0), align 8
  store i32 20, i32* getelementptr inbounds (%struct.FooBar, %struct.FooBar* @global_foo, i32 0, i32 1), align 8
  store i32 30, i32* getelementptr inbounds (%struct.FooBar, %struct.FooBar* @global_foo, i32 0, i32 2), align 8
  store i32 40, i32* getelementptr inbounds (%struct.FooBar, %struct.FooBar* @global_foo, i32 0, i32 3), align 8
  ret i32 0
}

declare i32 @rand() local_unnamed_addr

declare void @srand(i32)

declare i64 @time(i64*)

attributes #0 = { inlinehint }

!llvm.module.flags = !{!0}

!0 = !{i32 1, !"ProfileSummary", !1}
!1 = !{!2, !3, !4, !5, !6, !7, !8, !9}
!2 = !{!"ProfileFormat", !"InstrProf"}
!3 = !{!"TotalCount", i64 2736}
!4 = !{!"MaxCount", i64 1000}
!5 = !{!"MaxInternalCount", i64 1000}
!6 = !{!"MaxFunctionCount", i64 418}
!7 = !{!"NumCounts", i64 4}
!8 = !{!"NumFunctions", i64 1}
!9 = !{!"DetailedSummary", !10}
!10 = !{!11, !12, !13, !14, !15, !16, !16, !17, !17, !18, !19, !20, !21, !22, !23, !24, !25, !26}
!11 = !{i32 10000, i64 1000, i32 2}
!12 = !{i32 100000, i64 1000, i32 2}
!13 = !{i32 200000, i64 1000, i32 2}
!14 = !{i32 300000, i64 1000, i32 2}
!15 = !{i32 400000, i64 1000, i32 2}
!16 = !{i32 500000, i64 1000, i32 2}
!17 = !{i32 600000, i64 1000, i32 2}
!18 = !{i32 700000, i64 1000, i32 2}
!19 = !{i32 800000, i64 418, i32 3}
!20 = !{i32 900000, i64 318, i32 4}
!21 = !{i32 950000, i64 318, i32 4}
!22 = !{i32 990000, i64 318, i32 4}
!23 = !{i32 999000, i64 318, i32 4}
!24 = !{i32 999900, i64 318, i32 4}
!25 = !{i32 999990, i64 318, i32 4}
!26 = !{i32 999999, i64 318, i32 4}
!27 = !{!"function_entry_count", i64 1000}
!28 = !{!"branch_weights", i32 318, i32 1000}
!29 = !{!"branch_weights", i32 318, i32 418}
!30 = !{!"branch_weights", i32 418, i32 1000}
