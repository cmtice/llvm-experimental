; Test if FRG creation is correct or not.
; Create a simple for loop in the IR, with an if-else in the loop body. The test covers following attributes:
; Intra-BB creation:
; 1. Create a dummy node when a basic block doesn't have a field access or a memory access, e.g. Node 5, Node 8
; 2. Create a dummy node at the beginning of a BB if the first memory access is not a field access,
; e.g. Node 1 before Node 2, Node 6 before Node 7
; 3. Create only one node when a BB only has a field access, e.g. Node 4
; Inter-BB connection:
; 4. Record bytes accessed after the last field access in each basic block, e.g. Node 0 -> Node 1, Node 3 -> Node 4/5
; 5. Test the ability to connect a BB and its successors with branch probability, e.g. Node 2 -> Node 3/8, Node 3 -> Node 4/5
;
; RUN: llvm-as < %s > %t1
; RUN: llvm-lto -O0 -struct-field-cache-analysis -struct-analysis-FRG-only -o %t2 %t1 2>&1 | FileCheck %s
; CHECK: Field Reference Graph for function: main
; CHECK: Node 0 accesses 0 and has 3.00 out sum and 0.00 in sum: connect with { Node 1 (3.00,16.000)  }
; CHECK: Node 1 accesses 0 and has 303.00 out sum and 303.00 in sum: connect with { Node 2 (303.00,0.000)  }
; CHECK: Node 2 accesses 1 and has 303.00 out sum and 303.00 in sum: connect with { Node 8 (3.00,0.000)   Node 3 (300.00,0.000)  }
; CHECK: Node 8 accesses 0 and has 0.00 out sum and 3.00 in sum: connect with {}
; CHECK: Node 3 accesses 2 and has 298.00 out sum and 300.00 in sum: connect with { Node 5 (169.00,4.000)   Node 4 (129.00,4.000)  }
; CHECK: Node 5 accesses 0 and has 169.00 out sum and 169.00 in sum: connect with { Node 6 (169.00,0.000)  }
; CHECK: Node 4 accesses 2 and has 129.00 out sum and 129.00 in sum: connect with { Node 6 (129.00,0.000)  }
; CHECK: Node 6 accesses 0 and has 300.00 out sum and 298.00 in sum: connect with { Node 7 (300.00,4.000)  }
; CHECK: Node 7 accesses 3 and has 300.00 out sum and 300.00 in sum: connect with { Node 1 (300.00,0.000) B }

%struct.FooBar = type { i32, i8, double }

@global_foo = global %struct.FooBar zeroinitializer, align 8

; Function Attrs: inlinehint noinline norecurse nounwind uwtable
define i32 @main(i32 %argc, i8** %argv) #0 !prof !28 {
entry:
  %retval = alloca i32, align 4
  %argc.addr = alloca i32, align 4
  %argv.addr = alloca i8**, align 8
  %i = alloca i32, align 4
  store i32 0, i32* %retval, align 4
  store i32 %argc, i32* %argc.addr, align 4
  store i8** %argv, i8*** %argv.addr, align 8
  store i32 0, i32* %i, align 4
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %0 = load i32, i32* %i, align 4
  %1 = getelementptr inbounds %struct.FooBar, %struct.FooBar* @global_foo, i32 0, i32 0
  store i32 10, i32* %1, align 8
  %cmp = icmp ult i32 %0, 100
  br i1 %cmp, label %for.body, label %for.end, !prof !29

for.body:                                         ; preds = %for.cond
  store i8 65, i8* getelementptr inbounds (%struct.FooBar, %struct.FooBar* @global_foo, i32 0, i32 1), align 4
  %2 = load i32, i32* %i, align 4
  %cmp1 = icmp ult i32 %2, 50
  br i1 %cmp1, label %if.then, label %if.end, !prof !30

if.then:                                          ; preds = %for.body
  %3 = load i8, i8* getelementptr inbounds (%struct.FooBar, %struct.FooBar* @global_foo, i32 0, i32 1), align 4
  br label %for.inc

if.end:                                           ; preds = %for.body
  br label %for.inc

for.inc:                                          ; preds = %if.end, %if.then
  %4 = load i32, i32* %i, align 4
  %inc = add i32 %4, 1
  store i32 %inc, i32* %i, align 4
  store double 100.0, double* getelementptr inbounds (%struct.FooBar, %struct.FooBar* @global_foo, i32 0, i32 2), align 8
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
!4 = !{!"TotalCount", i64 303}
!5 = !{!"MaxCount", i64 170}
!6 = !{!"MaxInternalCount", i64 170}
!7 = !{!"MaxFunctionCount", i64 130}
!8 = !{!"NumCounts", i64 3}
!9 = !{!"NumFunctions", i64 1}
!10 = !{!"DetailedSummary", !11}
!11 = !{!12, !13, !14, !15, !16, !17, !17, !18, !18, !19, !20, !21, !22, !23, !24, !25, !26, !27}
!12 = !{i32 10000, i64 170, i32 1}
!13 = !{i32 100000, i64 170, i32 1}
!14 = !{i32 200000, i64 170, i32 1}
!15 = !{i32 300000, i64 170, i32 1}
!16 = !{i32 400000, i64 170, i32 1}
!17 = !{i32 500000, i64 170, i32 1}
!18 = !{i32 600000, i64 130, i32 2}
!19 = !{i32 700000, i64 130, i32 2}
!20 = !{i32 800000, i64 130, i32 2}
!21 = !{i32 900000, i64 130, i32 2}
!22 = !{i32 950000, i64 130, i32 2}
!23 = !{i32 990000, i64 130, i32 2}
!24 = !{i32 999000, i64 3, i32 3}
!25 = !{i32 999900, i64 3, i32 3}
!26 = !{i32 999990, i64 3, i32 3}
!27 = !{i32 999999, i64 3, i32 3}
!28 = !{!"function_entry_count", i64 3}
!29 = !{!"branch_weights", i32 300, i32 3}
!30 = !{!"branch_weights", i32 130, i32 170}
