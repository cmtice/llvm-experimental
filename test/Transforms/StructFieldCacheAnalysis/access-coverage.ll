; Test IR analysis coverage on types of accesses.
; There are a load and a store on a GEP inst and a load and a store on GEP operator. All four instructions
; should be detected. The GEP result also passed to a function, which will be detected as counted as the number
; of call/invoke instructions. GEP is also used in bitcast instruction, which should be counted.
;
; RUN: llvm-as < %s > %t1
; RUN: llvm-lto -O0 -struct-field-cache-analysis -struct-analysis-IR-only -o %t2 %t1 | FileCheck %s

; CHECK: There are 1 struct types are accessed in the program
; CHECK: Struct [struct.FooBar] defined as global struct has 7 accesses and 7 execution count.

%struct.FooBar = type { i32, i8, double }

@global_foo = global %struct.FooBar zeroinitializer, align 8

; Function Attrs: noinline nounwind uwtable
define void @_Z4funcPd(double* %C) #0 !prof !28 {
entry:
  %C.addr = alloca double*, align 8
  store double* %C, double** %C.addr, align 8
  %0 = load double*, double** %C.addr, align 8
  store double 1.000000e+02, double* %0, align 8
  ret void
}

; Function Attrs: noinline norecurse nounwind uwtable
define i32 @main(i32 %argc, i8** %argv) #1 !prof !28 {
entry:
  %retval = alloca i32, align 4
  %argc.addr = alloca i32, align 4
  %argv.addr = alloca i8**, align 8
  store i32 0, i32* %retval, align 4
  store i32 %argc, i32* %argc.addr, align 4
  store i8** %argv, i8*** %argv.addr, align 8
  %0 = getelementptr inbounds %struct.FooBar, %struct.FooBar* @global_foo, i32 0, i32 0
  store i32 10, i32* %0, align 8
  %1 = load i32, i32* %0, align 8
  store i8 65, i8* getelementptr inbounds (%struct.FooBar, %struct.FooBar* @global_foo, i32 0, i32 1), align 4
  %2 = load i8, i8* getelementptr inbounds (%struct.FooBar, %struct.FooBar* @global_foo, i32 0, i32 1), align 4
  %3 = bitcast i32* %0 to i64*
  store i64 10, i64* %3, align 8
  %4 = load i64, i64* %3, align 8
  call void @_Z4funcPd(double* getelementptr inbounds (%struct.FooBar, %struct.FooBar* @global_foo, i32 0, i32 2))
  ret i32 0
}

attributes #0 = { noinline nounwind uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { noinline norecurse nounwind uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = !{!"clang version 5.0.0 (git@github.com:cmtice/clang-experimental.git 3c6e6f19373f1bd0a6246346730fb82edbaa1c4a) (git@github.com:cmtice/llvm-experimental.git 68fd8a2f7d84272f67f2614bb3da3583af72ff4b)"}
!1 = !{i32 1, !"ProfileSummary", !2}
!2 = !{!3, !4, !5, !6, !7, !8, !9, !10}
!3 = !{!"ProfileFormat", !"InstrProf"}
!4 = !{!"TotalCount", i64 2}
!5 = !{!"MaxCount", i64 1}
!6 = !{!"MaxInternalCount", i64 0}
!7 = !{!"MaxFunctionCount", i64 1}
!8 = !{!"NumCounts", i64 2}
!9 = !{!"NumFunctions", i64 2}
!10 = !{!"DetailedSummary", !11}
!11 = !{!12, !13, !14, !15, !16, !17, !17, !18, !18, !19, !20, !21, !22, !23, !24, !25, !26, !27}
!12 = !{i32 10000, i64 0, i32 0}
!13 = !{i32 100000, i64 0, i32 0}
!14 = !{i32 200000, i64 0, i32 0}
!15 = !{i32 300000, i64 0, i32 0}
!16 = !{i32 400000, i64 0, i32 0}
!17 = !{i32 500000, i64 1, i32 2}
!18 = !{i32 600000, i64 1, i32 2}
!19 = !{i32 700000, i64 1, i32 2}
!20 = !{i32 800000, i64 1, i32 2}
!21 = !{i32 900000, i64 1, i32 2}
!22 = !{i32 950000, i64 1, i32 2}
!23 = !{i32 990000, i64 1, i32 2}
!24 = !{i32 999000, i64 1, i32 2}
!25 = !{i32 999900, i64 1, i32 2}
!26 = !{i32 999990, i64 1, i32 2}
!27 = !{i32 999999, i64 1, i32 2}
!28 = !{!"function_entry_count", i64 1}