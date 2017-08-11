; Test IR handling field address passed to functions.
; There are three function calls in the program. The first one is a traditional function calls
; and it should be detected. The second one is a call to an indirect function. Compiler should
; bail out and report in stats. The last one is a call to a function with unlimited arguments.
; Compiler should also bail out.
;
; RUN: llvm-as < %s > %t1
; RUN: llvm-lto -O0 -struct-field-cache-analysis -struct-analysis-IR-only -o %t2 %t1 | FileCheck %s
; CHECK: There are 1 struct types are accessed in the program
; CHECK: Struct [struct.FooBar] defined as global struct has 1 accesses and 1 execution count.
; CHECK: Case GEP value passed into indirect function calls or function that has undetermined num args was found 2 times

%struct.FooBar = type { i32, i8, double }

@global_foo = global %struct.FooBar zeroinitializer, align 8
@indirect_function = global void (i8*)* null, align 8
@.str = private unnamed_addr constant [3 x i8] c"%f\00", align 1

; Function Attrs: inlinehint noinline nounwind uwtable
define void @_Z15normal_functionPi(i32* %A) #0 !prof !28 {
entry:
  %A.addr = alloca i32*, align 8
  store i32* %A, i32** %A.addr, align 8
  %0 = load i32*, i32** %A.addr, align 8
  store i32 10, i32* %0, align 4
  ret void
}

; Function Attrs: inlinehint noinline nounwind uwtable
define void @_Z27indirect_function_candidatePc(i8* %B) #0 !prof !28 {
entry:
  %B.addr = alloca i8*, align 8
  store i8* %B, i8** %B.addr, align 8
  %0 = load i8*, i8** %B.addr, align 8
  store i8 65, i8* %0, align 1
  ret void
}

; Function Attrs: inlinehint noinline nounwind uwtable
define void @_Z29function_with_undermined_argsPKcz(i8* %Format, ...) #0 !prof !28 {
entry:
  %Format.addr = alloca i8*, align 8
  store i8* %Format, i8** %Format.addr, align 8
  ret void
}

; Function Attrs: inlinehint noinline norecurse uwtable
define i32 @main(i32 %argc, i8** %argv) #1 !prof !28 {
entry:
  %retval = alloca i32, align 4
  %argc.addr = alloca i32, align 4
  %argv.addr = alloca i8**, align 8
  store i32 0, i32* %retval, align 4
  store i32 %argc, i32* %argc.addr, align 4
  store i8** %argv, i8*** %argv.addr, align 8
  call void @_Z15normal_functionPi(i32* getelementptr inbounds (%struct.FooBar, %struct.FooBar* @global_foo, i32 0, i32 0))
  store void (i8*)* @_Z27indirect_function_candidatePc, void (i8*)** @indirect_function, align 8
  %0 = load void (i8*)*, void (i8*)** @indirect_function, align 8
  call void %0(i8* getelementptr inbounds (%struct.FooBar, %struct.FooBar* @global_foo, i32 0, i32 1)), !prof !29
  call void (i8*, ...) @_Z29function_with_undermined_argsPKcz(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str, i32 0, i32 0), double* getelementptr inbounds (%struct.FooBar, %struct.FooBar* @global_foo, i32 0, i32 2))
  ret i32 0
}

attributes #0 = { inlinehint noinline nounwind uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { inlinehint noinline norecurse uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}
!llvm.module.flags = !{!1}

!0 = !{!"clang version 5.0.0 (git@github.com:cmtice/clang-experimental.git 3c6e6f19373f1bd0a6246346730fb82edbaa1c4a) (git@github.com:cmtice/llvm-experimental.git 3c29fb0ca228e8e466433a617d2fc5f7766ac692)"}
!1 = !{i32 1, !"ProfileSummary", !2}
!2 = !{!3, !4, !5, !6, !7, !8, !9, !10}
!3 = !{!"ProfileFormat", !"InstrProf"}
!4 = !{!"TotalCount", i64 4}
!5 = !{!"MaxCount", i64 1}
!6 = !{!"MaxInternalCount", i64 0}
!7 = !{!"MaxFunctionCount", i64 1}
!8 = !{!"NumCounts", i64 4}
!9 = !{!"NumFunctions", i64 4}
!10 = !{!"DetailedSummary", !11}
!11 = !{!12, !13, !14, !15, !16, !17, !17, !18, !18, !19, !20, !21, !22, !23, !24, !25, !26, !27}
!12 = !{i32 10000, i64 0, i32 0}
!13 = !{i32 100000, i64 0, i32 0}
!14 = !{i32 200000, i64 0, i32 0}
!15 = !{i32 300000, i64 1, i32 4}
!16 = !{i32 400000, i64 1, i32 4}
!17 = !{i32 500000, i64 1, i32 4}
!18 = !{i32 600000, i64 1, i32 4}
!19 = !{i32 700000, i64 1, i32 4}
!20 = !{i32 800000, i64 1, i32 4}
!21 = !{i32 900000, i64 1, i32 4}
!22 = !{i32 950000, i64 1, i32 4}
!23 = !{i32 990000, i64 1, i32 4}
!24 = !{i32 999000, i64 1, i32 4}
!25 = !{i32 999900, i64 1, i32 4}
!26 = !{i32 999990, i64 1, i32 4}
!27 = !{i32 999999, i64 1, i32 4}
!28 = !{!"function_entry_count", i64 1}
!29 = !{!"VP", i32 0, i64 1, i64 3323131702599207299, i64 1}
