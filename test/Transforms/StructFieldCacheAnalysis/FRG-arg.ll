; Test if FRG analysis can handle field addresses passed as function arguments
; There are two functions defined in the program: normal_function and normal_function2
; The first function is called three times and each time one field is passed into the
; function. With current implementation, it should be ignored because we only support
; one argument being one field in the program. Meanwhile, normal_function2 is called once
; with one field. This case should be handled.
;
; RUN: llvm-as < %s > %t1
; RUN: llvm-lto -O0 -struct-field-cache-analysis -struct-analysis-CPG-only -struct-analysis-check-CPG -o %t2 %t1 2>&1 | FileCheck %s
; CHECK: There are 1 struct types are accessed in the program
; CHECK: Struct [struct.FooBar] defined as global struct has 6 accesses and 180 execution count.
; CHECK: Case Different function calls taking different arguments found was found 1 times
; CHECK-NOT: Found error in CPG

%struct.FooBar = type { i32, i32, i32 }

@global_foo = global %struct.FooBar zeroinitializer, align 4

; Function Attrs: inlinehint noinline nounwind uwtable
define void @_Z15normal_functionPi(i32* %Field) #0 !prof !28 {
entry:
  %Field.addr = alloca i32*, align 8
  store i32* %Field, i32** %Field.addr, align 8
  %0 = load i32*, i32** %Field.addr, align 8
  %1 = load i32, i32* %0, align 4
  %cmp = icmp eq i32 %1, 0
  br i1 %cmp, label %if.then, label %if.else, !prof !29

if.then:                                          ; preds = %entry
  %2 = load i32*, i32** %Field.addr, align 8
  store i32 10, i32* %2, align 4
  br label %if.end

if.else:                                          ; preds = %entry
  %3 = load i32*, i32** %Field.addr, align 8
  %4 = load i32, i32* %3, align 4
  %inc = add nsw i32 %4, 1
  store i32 %inc, i32* %3, align 4
  br label %if.end

if.end:                                           ; preds = %if.else, %if.then
  ret void
}

; Function Attrs: inlinehint noinline nounwind uwtable
define void @_Z16normal_function2Pi(i32* %Field) #0 !prof !30 {
entry:
  %Field.addr = alloca i32*, align 8
  store i32* %Field, i32** %Field.addr, align 8
  %0 = load i32*, i32** %Field.addr, align 8
  %1 = load i32, i32* %0, align 4
  %cmp = icmp eq i32 %1, 0
  br i1 %cmp, label %if.then, label %if.else, !prof !31

if.then:                                          ; preds = %entry
  %2 = load i32*, i32** %Field.addr, align 8
  store i32 20, i32* %2, align 4
  br label %if.end3

if.else:                                          ; preds = %entry
  %3 = load i32*, i32** %Field.addr, align 8
  %4 = load i32, i32* %3, align 4
  %inc = add nsw i32 %4, 1
  store i32 %inc, i32* %3, align 4
  %5 = load i32*, i32** %Field.addr, align 8
  %6 = load i32, i32* %5, align 4
  %cmp1 = icmp sgt i32 %6, 5
  br i1 %cmp1, label %if.then2, label %if.end, !prof !32

if.then2:                                         ; preds = %if.else
  store i32 10, i32* getelementptr inbounds (%struct.FooBar, %struct.FooBar* @global_foo, i32 0, i32 1), align 4
  store i32 20, i32* getelementptr inbounds (%struct.FooBar, %struct.FooBar* @global_foo, i32 0, i32 2), align 4
  br label %if.end

if.end:                                           ; preds = %if.then2, %if.else
  br label %if.end3

if.end3:                                          ; preds = %if.end, %if.then
  ret void
}

; Function Attrs: inlinehint noinline norecurse nounwind uwtable
define i32 @main(i32 %argc, i8** %argv) #1 !prof !33 {
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
  %cmp = icmp ult i32 %0, 100
  br i1 %cmp, label %for.body, label %for.end, !prof !34

for.body:                                         ; preds = %for.cond
  %1 = load i32, i32* %i, align 4
  %cmp1 = icmp ult i32 %1, 20
  br i1 %cmp1, label %if.then, label %if.else, !prof !35

if.then:                                          ; preds = %for.body
  call void @_Z15normal_functionPi(i32* getelementptr inbounds (%struct.FooBar, %struct.FooBar* @global_foo, i32 0, i32 0))
  br label %if.end9

if.else:                                          ; preds = %for.body
  %2 = load i32, i32* %i, align 4
  %cmp2 = icmp ult i32 %2, 40
  br i1 %cmp2, label %if.then3, label %if.else4, !prof !36

if.then3:                                         ; preds = %if.else
  call void @_Z15normal_functionPi(i32* getelementptr inbounds (%struct.FooBar, %struct.FooBar* @global_foo, i32 0, i32 1))
  br label %if.end8

if.else4:                                         ; preds = %if.else
  %3 = load i32, i32* %i, align 4
  %cmp5 = icmp ult i32 %3, 60
  br i1 %cmp5, label %if.then6, label %if.else7, !prof !37

if.then6:                                         ; preds = %if.else4
  call void @_Z15normal_functionPi(i32* getelementptr inbounds (%struct.FooBar, %struct.FooBar* @global_foo, i32 0, i32 2))
  br label %if.end

if.else7:                                         ; preds = %if.else4
  call void @_Z16normal_function2Pi(i32* getelementptr inbounds (%struct.FooBar, %struct.FooBar* @global_foo, i32 0, i32 0))
  br label %if.end

if.end:                                           ; preds = %if.else7, %if.then6
  br label %if.end8

if.end8:                                          ; preds = %if.end, %if.then3
  br label %if.end9

if.end9:                                          ; preds = %if.end8, %if.then
  br label %for.inc

for.inc:                                          ; preds = %if.end9
  %4 = load i32, i32* %i, align 4
  %inc = add i32 %4, 1
  store i32 %inc, i32* %i, align 4
  br label %for.cond

for.end:                                          ; preds = %for.cond
  ret i32 0
}

attributes #0 = { inlinehint noinline nounwind uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { inlinehint noinline norecurse nounwind uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}
!llvm.module.flags = !{!1}

!0 = !{!"clang version 5.0.0 (git@github.com:cmtice/clang-experimental.git 3c6e6f19373f1bd0a6246346730fb82edbaa1c4a) (git@github.com:cmtice/llvm-experimental.git 3c29fb0ca228e8e466433a617d2fc5f7766ac692)"}
!1 = !{i32 1, !"ProfileSummary", !2}
!2 = !{!3, !4, !5, !6, !7, !8, !9, !10}
!3 = !{!"ProfileFormat", !"InstrProf"}
!4 = !{!"TotalCount", i64 241}
!5 = !{!"MaxCount", i64 57}
!6 = !{!"MaxInternalCount", i64 40}
!7 = !{!"MaxFunctionCount", i64 57}
!8 = !{!"NumCounts", i64 10}
!9 = !{!"NumFunctions", i64 3}
!10 = !{!"DetailedSummary", !11}
!11 = !{!12, !13, !14, !15, !16, !17, !17, !18, !18, !19, !20, !21, !22, !23, !24, !25, !26, !27}
!12 = !{i32 10000, i64 57, i32 1}
!13 = !{i32 100000, i64 57, i32 1}
!14 = !{i32 200000, i64 57, i32 1}
!15 = !{i32 300000, i64 40, i32 4}
!16 = !{i32 400000, i64 40, i32 4}
!17 = !{i32 500000, i64 40, i32 4}
!18 = !{i32 600000, i64 40, i32 4}
!19 = !{i32 700000, i64 40, i32 4}
!20 = !{i32 800000, i64 20, i32 7}
!21 = !{i32 900000, i64 20, i32 7}
!22 = !{i32 950000, i64 20, i32 7}
!23 = !{i32 990000, i64 3, i32 8}
!24 = !{i32 999000, i64 3, i32 8}
!25 = !{i32 999900, i64 3, i32 8}
!26 = !{i32 999990, i64 3, i32 8}
!27 = !{i32 999999, i64 3, i32 8}
!28 = !{!"function_entry_count", i64 60}
!29 = !{!"branch_weights", i32 3, i32 57}
!30 = !{!"function_entry_count", i64 40}
!31 = !{!"branch_weights", i32 0, i32 40}
!32 = !{!"branch_weights", i32 40, i32 0}
!33 = !{!"function_entry_count", i64 1}
!34 = !{!"branch_weights", i32 100, i32 1}
!35 = !{!"branch_weights", i32 20, i32 80}
!36 = !{!"branch_weights", i32 20, i32 60}
!37 = !{!"branch_weights", i32 20, i32 40}
