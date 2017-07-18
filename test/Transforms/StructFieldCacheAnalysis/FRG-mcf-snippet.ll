; Test if FRG creation is correct or not.
; The IR is taken from mcf benchmark in SPEC2000. It was failed before because BB591 belongs to two different loops and
; it failed to detect the backedge from BB591 to BB581. The test is created to make sure the problem solved.
;
; RUN: llvm-as < %s > %t1
; RUN: llvm-lto -O0 -struct-field-cache-analysis -struct-analysis-FRG-only -struct-analysis-disable-ignore-zeros -struct-analysis-CPG-only -o %t2 %t1 2>&1 | FileCheck %s
; CHECK: Field Reference Graph for function: main
; Node 0 accesses 0 and has 1.00 out sum and 0.00 in sum: connect with { Node 1 (1.00,16.000)  }
; Node 1 accesses 1 and has 1.71 out sum and 1.50 in sum: connect with { Node 2 (1.71,4.000)  }
; Node 2 accesses 0 and has 2.00 out sum and 1.71 in sum: connect with { Node 3 (1.00,0.000)   Node 5 (1.00,0.000)  }
; Node 3 accesses 2 and has 1.00 out sum and 2.00 in sum: connect with { Node 1 (0.50,4.000) B  Node 4 (0.50,4.000)  }
; Node 5 accesses 0 and has 0.00 out sum and 1.00 in sum: connect with {}
; Node 4 accesses 3 and has 1.00 out sum and 0.50 in sum: connect with { Node 3 (1.00,4.000) B  Node 5 (0.00,4.000)  }

%struct.FooBar = type { i32, i8, double }

@global_foo = global %struct.FooBar zeroinitializer, align 8

; Function Attrs: inlinehint
define i32 @main(i32 %argc, i8** %argv) #0 !prof !27 {
entry:
  %retval = alloca i32, align 4
  %argc.addr = alloca i32, align 4
  %argv.addr = alloca i8**, align 8
  %i = alloca i32, align 4
  store i32 0, i32* %retval, align 4
  store i32 %argc, i32* %argc.addr, align 4
  store i8** %argv, i8*** %argv.addr, align 8
  store i32 0, i32* %i, align 4
  br label %BB581

BB581:                                            ; preds = %BB581, %BB591, %entry
  %0 = getelementptr inbounds %struct.FooBar, %struct.FooBar* @global_foo, i32 0, i32 0
  store i32 10, i32* %0, align 8
  %1 = call i32 @rand()
  %2 = srem i32 %1, 50
  %3 = load i32, i32* %i, align 4
  %cmp1 = icmp ult i32 %2, 20
  br i1 %cmp1, label %BB589, label %BB581, !prof !28

BB589:                                            ; preds = %BB581
  %4 = load i32, i32* %i, align 4
  %5 = call i32 @rand()
  %6 = srem i32 %5, 50
  %cmp2 = icmp ult i32 %6, 30
  br i1 %cmp2, label %BB600, label %BB591, !prof !29

BB591:                                            ; preds = %BB596, %BB589
  store i8 65, i8* getelementptr inbounds (%struct.FooBar, %struct.FooBar* @global_foo, i32 0, i32 1), align 4
  %7 = call i32 @rand()
  %8 = srem i32 %7, 50
  %9 = load i32, i32* %i, align 4
  %cmp3 = icmp ult i32 %8, 40
  br i1 %cmp3, label %BB596, label %BB581, !prof !29

BB596:                                            ; preds = %BB591
  store double 1.000000e+02, double* getelementptr inbounds (%struct.FooBar, %struct.FooBar* @global_foo, i32 0, i32 2), align 8
  %10 = call i32 @rand()
  %11 = srem i32 %10, 50
  %12 = load i32, i32* %i, align 4
  %cmp4 = icmp ult i32 %11, 25
  br i1 %cmp4, label %BB600, label %BB591, !prof !30

BB600:                                            ; preds = %BB589, %BB596
  ret i32 0
}

declare i32 @rand() local_unnamed_addr

attributes #0 = { inlinehint }

!llvm.module.flags = !{!0}

!0 = !{i32 1, !"ProfileSummary", !1}
!1 = !{!2, !3, !4, !5, !6, !7, !8, !9}
!2 = !{!"ProfileFormat", !"InstrProf"}
!3 = !{!"TotalCount", i64 10}
!4 = !{!"MaxCount", i64 5}
!5 = !{!"MaxInternalCount", i64 2}
!6 = !{!"MaxFunctionCount", i64 5}
!7 = !{!"NumCounts", i64 5}
!8 = !{!"NumFunctions", i64 1}
!9 = !{!"DetailedSummary", !10}
!10 = !{!11, !12, !13, !14, !15, !16, !16, !17, !17, !18, !19, !20, !21, !22, !23, !24, !25, !26}
!11 = !{i32 10000, i64 0, i32 0}
!12 = !{i32 100000, i64 5, i32 1}
!13 = !{i32 200000, i64 5, i32 1}
!14 = !{i32 300000, i64 5, i32 1}
!15 = !{i32 400000, i64 5, i32 1}
!16 = !{i32 500000, i64 5, i32 1}
!17 = !{i32 600000, i64 2, i32 2}
!18 = !{i32 700000, i64 2, i32 2}
!19 = !{i32 800000, i64 1, i32 5}
!20 = !{i32 900000, i64 1, i32 5}
!21 = !{i32 950000, i64 1, i32 5}
!22 = !{i32 990000, i64 1, i32 5}
!23 = !{i32 999000, i64 1, i32 5}
!24 = !{i32 999900, i64 1, i32 5}
!25 = !{i32 999990, i64 1, i32 5}
!26 = !{i32 999999, i64 1, i32 5}
!27 = !{!"function_entry_count", i64 1}
!28 = !{!"branch_weights", i32 2, i32 5}
!29 = !{!"branch_weights", i32 1, i32 1}
!30 = !{!"branch_weights", i32 0, i32 1}
