; Test if CPG collapsing algorithm is correct on a more complicate IR
; This test checks if the count is handled properly on nodes that has multiple in-edges and multiple out-edges
; e.g. Node 3 (BB3) and Node 4 (BB4)
; This test only needs to detect if there's error reported.
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

BB1:                                              ; preds = %entry
  store i32 20, i32* getelementptr inbounds (%struct.FooBar, %struct.FooBar* @global_foo, i32 0, i32 1), align 8
  %call1 = call i32 @rand()
  %rand1 = srem i32 %call1, 50
  %cmp1 = icmp ult i32 %rand1, 25
  br i1 %cmp1, label %BB2, label %BB3, !prof !28

BB2:                                              ; preds = %BB3, %BB1
  store i32 30, i32* getelementptr inbounds (%struct.FooBar, %struct.FooBar* @global_foo, i32 0, i32 2), align 8
  %call2 = call i32 @rand()
  %rand2 = srem i32 %call2, 50
  %cmp2 = icmp ult i32 %rand2, 25
  br i1 %cmp2, label %BB3, label %BB4, !prof !29

BB3:                                              ; preds = %BB2, %BB1
  store i32 40, i32* getelementptr inbounds (%struct.FooBar, %struct.FooBar* @global_foo, i32 0, i32 3), align 8
  %call3 = call i32 @rand()
  %rand3 = srem i32 %call3, 50
  %cmp3 = icmp ult i32 %rand3, 25
  br i1 %cmp3, label %BB2, label %BB4, !prof !30

BB4:                                              ; preds = %BB2, %BB3
  store i32 10, i32* getelementptr inbounds (%struct.FooBar, %struct.FooBar* @global_foo, i32 0, i32 0), align 8
  %call4 = call i32 @rand()
  %rand4 = srem i32 %call4, 50
  %cmp4 = icmp ult i32 %rand4, 10
  br i1 %cmp4, label %BB5, label %BB6, !prof !31

BB5:                                              ; preds = %BB4
  store i32 20, i32* getelementptr inbounds (%struct.FooBar, %struct.FooBar* @global_foo, i32 0, i32 1), align 8
  br label %BB7

BB6:                                              ; preds = %BB4
  store i32 30, i32* getelementptr inbounds (%struct.FooBar, %struct.FooBar* @global_foo, i32 0, i32 2), align 8
  br label %BB7

BB7:                                              ; preds = %BB6, %BB5
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
!3 = !{!"TotalCount", i64 2858}
!4 = !{!"MaxCount", i64 1247}
!5 = !{!"MaxInternalCount", i64 1247}
!6 = !{!"MaxFunctionCount", i64 611}
!7 = !{!"NumCounts", i64 5}
!8 = !{!"NumFunctions", i64 1}
!9 = !{!"DetailedSummary", !10}
!10 = !{!11, !12, !13, !14, !15, !16, !16, !17, !17, !18, !19, !20, !21, !22, !23, !24, !25, !26}
!11 = !{i32 10000, i64 1247, i32 1}
!12 = !{i32 100000, i64 1247, i32 1}
!13 = !{i32 200000, i64 1247, i32 1}
!14 = !{i32 300000, i64 1247, i32 1}
!15 = !{i32 400000, i64 1247, i32 1}
!16 = !{i32 500000, i64 1000, i32 2}
!17 = !{i32 600000, i64 1000, i32 2}
!18 = !{i32 700000, i64 1000, i32 2}
!19 = !{i32 800000, i64 611, i32 3}
!20 = !{i32 900000, i64 611, i32 3}
!21 = !{i32 950000, i64 611, i32 3}
!22 = !{i32 990000, i64 611, i32 3}
!23 = !{i32 999000, i64 611, i32 3}
!24 = !{i32 999900, i64 611, i32 3}
!25 = !{i32 999990, i64 611, i32 3}
!26 = !{i32 999999, i64 611, i32 3}
!27 = !{!"function_entry_count", i64 1000}
!28 = !{!"branch_weights", i32 636, i32 364}
!29 = !{!"branch_weights", i32 1336, i32 300}
!30 = !{!"branch_weights", i32 1000, i32 700}
!31 = !{!"branch_weights", i32 831, i32 169}
