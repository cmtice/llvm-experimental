; Test if FRG creation is correct or not when there are multiple edges between two nodes
;
; RUN: llvm-as < %s > %t1
; RUN: llvm-lto -O0 -struct-field-cache-analysis -struct-analysis-FRG-only -struct-analysis-CPG-only -o %t2 %t1 2>&1 | FileCheck %s
; CHECK: Field Reference Graph for function: main
; CHECK: Node 0 accesses F1 and has 1000.00 out sum and 0.00 in sum: connect with { Node 1 (1000.00,0.000)  }
; CHECK: Node 1 accesses F2 and has 2333.00 out sum and 1000.00 in sum: connect with { Node 3 (2229.00,0.000)   Node 2 (104.00,0.000)  }
; CHECK: Node 3 accesses F4 and has 0.00 out sum and 2333.00 in sum: connect with {}
; CHECK: Node 2 accesses F3 and has 104.00 out sum and 104.00 in sum: connect with { Node 3 (104.00,0.000)  }

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
  %rand1 = srem i32 %call1, 4
  %result = add i32 %rand1, 2
  %cmp1 = trunc i32 %result to i2
  switch i2 %cmp1, label %BB2 [
    i2 -2, label %BB3
    i2 -1, label %BB3
    i2 0, label %BB3
  ], !prof !28

BB2:                                              ; preds = %BB1
  store i32 30, i32* getelementptr inbounds (%struct.FooBar, %struct.FooBar* @global_foo, i32 0, i32 2), align 8
  br label %BB3

BB3:                                              ; preds = %BB1, %BB1, %BB2, %BB1
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
!3 = !{!"TotalCount", i64 1784}
!4 = !{!"MaxCount", i64 1000}
!5 = !{!"MaxInternalCount", i64 1000}
!6 = !{!"MaxFunctionCount", i64 527}
!7 = !{!"NumCounts", i64 4}
!8 = !{!"NumFunctions", i64 1}
!9 = !{!"DetailedSummary", !10}
!10 = !{!11, !12, !13, !14, !15, !16, !16, !17, !17, !18, !19, !20, !21, !22, !23, !24, !25, !26}
!11 = !{i32 10000, i64 1000, i32 1}
!12 = !{i32 100000, i64 1000, i32 1}
!13 = !{i32 200000, i64 1000, i32 1}
!14 = !{i32 300000, i64 1000, i32 1}
!15 = !{i32 400000, i64 1000, i32 1}
!16 = !{i32 500000, i64 1000, i32 1}
!17 = !{i32 600000, i64 527, i32 2}
!18 = !{i32 700000, i64 527, i32 2}
!19 = !{i32 800000, i64 527, i32 2}
!20 = !{i32 900000, i64 257, i32 3}
!21 = !{i32 950000, i64 257, i32 3}
!22 = !{i32 990000, i64 257, i32 3}
!23 = !{i32 999000, i64 257, i32 3}
!24 = !{i32 999900, i64 257, i32 3}
!25 = !{i32 999990, i64 257, i32 3}
!26 = !{i32 999999, i64 257, i32 3}
!27 = !{!"function_entry_count", i64 1000}
!28 = !{!"branch_weights", i32 257, i32 527, i32 0, i32 216}
