; Test IR analysis coverage. There are 6 struct types, and each of them are global struct, global struct*
; global struct[], local struct, local struct* and local struct* but assigned an array
; Each of them are accessed once in main() and the two pointer kinds are also passed to a function
; and accessed onces more
; All of the array type should be detected. So all six structs will be printed out.
;
; RUN: llvm-as < %s > %t1
; RUN: llvm-lto -O0 -struct-field-cache-analysis -struct-analysis-IR-only -o %t2 %t1 | FileCheck %s
; CHECK: There are 6 struct types are accessed in the program
; CHECK: Struct [struct.FooBar4] defined as local struct has 1 accesses and 0 execution count.
; CHECK: Struct [struct.FooBar5] defined as local struct* has 2 accesses and 0 execution count.
; CHECK: Struct [struct.FooBar3] defined as global struct[] has 1 accesses and 0 execution count.
; CHECK: Struct [struct.FooBar2] defined as global struct* has 2 accesses and 0 execution count.
; CHECK: Struct [struct.FooBar6] defined as local struct* has 1 accesses and 0 execution count.
; CHECK: Struct [struct.FooBar1] defined as global struct has 1 accesses and 0 execution count.

%struct.FooBar1 = type { i32 }
%struct.FooBar2 = type { float }
%struct.FooBar3 = type { i64 }
%struct.FooBar5 = type { i16 }
%struct.FooBar4 = type { i8 }
%struct.FooBar6 = type { double }

@global_foo = global %struct.FooBar1 zeroinitializer, align 4
@global_foo_ptr = global %struct.FooBar2* null, align 8
@global_foo_array = global [5 x %struct.FooBar3] zeroinitializer, align 16

; Function Attrs: noinline nounwind uwtable
define void @_Z11access_funcP7FooBar2(%struct.FooBar2* %foo) #0 {
entry:
  %foo.addr = alloca %struct.FooBar2*, align 8
  store %struct.FooBar2* %foo, %struct.FooBar2** %foo.addr, align 8
  %0 = load %struct.FooBar2*, %struct.FooBar2** %foo.addr, align 8
  %A = getelementptr inbounds %struct.FooBar2, %struct.FooBar2* %0, i32 0, i32 0
  store float 9.700000e+01, float* %A, align 4
  ret void
}

; Function Attrs: noinline nounwind uwtable
define void @_Z11access_funcP7FooBar5(%struct.FooBar5* %foo) #0 {
entry:
  %foo.addr = alloca %struct.FooBar5*, align 8
  store %struct.FooBar5* %foo, %struct.FooBar5** %foo.addr, align 8
  %0 = load %struct.FooBar5*, %struct.FooBar5** %foo.addr, align 8
  %A = getelementptr inbounds %struct.FooBar5, %struct.FooBar5* %0, i32 0, i32 0
  store i16 97, i16* %A, align 2
  ret void
}

; Function Attrs: noinline norecurse uwtable
define i32 @main(i32 %argc, i8** %argv) #1 {
entry:
  %retval = alloca i32, align 4
  %argc.addr = alloca i32, align 4
  %argv.addr = alloca i8**, align 8
  %tmp = alloca i32, align 4
  %tmp1 = alloca float, align 4
  %i = alloca i32, align 4
  %tmp3 = alloca i64, align 8
  %local_foo = alloca %struct.FooBar4, align 1
  %local_foo_ptr = alloca %struct.FooBar5*, align 8
  %local_foo_array = alloca %struct.FooBar6*, align 8
  %tmp10 = alloca i8, align 1
  %tmp12 = alloca i16, align 2
  %i15 = alloca i32, align 4
  %tmp19 = alloca double, align 8
  store i32 0, i32* %retval, align 4
  store i32 %argc, i32* %argc.addr, align 4
  store i8** %argv, i8*** %argv.addr, align 8
  %call = call i8* @_Znwm(i64 4) #3
  %0 = bitcast i8* %call to %struct.FooBar2*
  store %struct.FooBar2* %0, %struct.FooBar2** @global_foo_ptr, align 8
  store i32 65, i32* getelementptr inbounds (%struct.FooBar1, %struct.FooBar1* @global_foo, i32 0, i32 0), align 4
  store i32 65, i32* %tmp, align 4
  %1 = load i32, i32* %tmp, align 4
  %2 = load %struct.FooBar2*, %struct.FooBar2** @global_foo_ptr, align 8
  %A = getelementptr inbounds %struct.FooBar2, %struct.FooBar2* %2, i32 0, i32 0
  store float 6.500000e+01, float* %A, align 4
  store float 6.500000e+01, float* %tmp1, align 4
  %3 = load float, float* %tmp1, align 4
  store i32 0, i32* %i, align 4
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %4 = load i32, i32* %i, align 4
  %cmp = icmp ult i32 %4, 5
  br i1 %cmp, label %for.body, label %for.end

for.body:                                         ; preds = %for.cond
  %5 = load i32, i32* %i, align 4
  %idxprom = zext i32 %5 to i64
  %arrayidx = getelementptr inbounds [5 x %struct.FooBar3], [5 x %struct.FooBar3]* @global_foo_array, i64 0, i64 %idxprom
  %A4 = getelementptr inbounds %struct.FooBar3, %struct.FooBar3* %arrayidx, i32 0, i32 0
  store i64 65, i64* %A4, align 8
  store i64 65, i64* %tmp3, align 8
  %6 = load i64, i64* %tmp3, align 8
  br label %for.inc

for.inc:                                          ; preds = %for.body
  %7 = load i32, i32* %i, align 4
  %inc = add i32 %7, 1
  store i32 %inc, i32* %i, align 4
  br label %for.cond

for.end:                                          ; preds = %for.cond
  %8 = load %struct.FooBar2*, %struct.FooBar2** @global_foo_ptr, align 8
  call void @_Z11access_funcP7FooBar2(%struct.FooBar2* %8)
  %call7 = call i8* @_Znwm(i64 2) #3
  %9 = bitcast i8* %call7 to %struct.FooBar5*
  store %struct.FooBar5* %9, %struct.FooBar5** %local_foo_ptr, align 8
  %call9 = call i8* @_Znam(i64 40) #3
  %10 = bitcast i8* %call9 to %struct.FooBar6*
  store %struct.FooBar6* %10, %struct.FooBar6** %local_foo_array, align 8
  %A11 = getelementptr inbounds %struct.FooBar4, %struct.FooBar4* %local_foo, i32 0, i32 0
  store i8 65, i8* %A11, align 1
  store i8 65, i8* %tmp10, align 1
  %11 = load i8, i8* %tmp10, align 1
  %12 = load %struct.FooBar5*, %struct.FooBar5** %local_foo_ptr, align 8
  %A13 = getelementptr inbounds %struct.FooBar5, %struct.FooBar5* %12, i32 0, i32 0
  store i16 65, i16* %A13, align 2
  store i16 65, i16* %tmp12, align 2
  %13 = load i16, i16* %tmp12, align 2
  store i32 0, i32* %i15, align 4
  br label %for.cond16

for.cond16:                                       ; preds = %for.inc23, %for.end
  %14 = load i32, i32* %i15, align 4
  %cmp17 = icmp ult i32 %14, 5
  br i1 %cmp17, label %for.body18, label %for.end25

for.body18:                                       ; preds = %for.cond16
  %15 = load %struct.FooBar6*, %struct.FooBar6** %local_foo_array, align 8
  %16 = load i32, i32* %i15, align 4
  %idxprom20 = zext i32 %16 to i64
  %arrayidx21 = getelementptr inbounds %struct.FooBar6, %struct.FooBar6* %15, i64 %idxprom20
  %A22 = getelementptr inbounds %struct.FooBar6, %struct.FooBar6* %arrayidx21, i32 0, i32 0
  store double 6.500000e+01, double* %A22, align 8
  store double 6.500000e+01, double* %tmp19, align 8
  %17 = load double, double* %tmp19, align 8
  br label %for.inc23

for.inc23:                                        ; preds = %for.body18
  %18 = load i32, i32* %i15, align 4
  %inc24 = add i32 %18, 1
  store i32 %inc24, i32* %i15, align 4
  br label %for.cond16

for.end25:                                        ; preds = %for.cond16
  %19 = load %struct.FooBar5*, %struct.FooBar5** %local_foo_ptr, align 8
  call void @_Z11access_funcP7FooBar5(%struct.FooBar5* %19)
  ret i32 0
}

; Function Attrs: nobuiltin
declare noalias i8* @_Znwm(i64) #2

; Function Attrs: nobuiltin
declare noalias i8* @_Znam(i64) #2

attributes #0 = { noinline nounwind uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { noinline norecurse uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { nobuiltin "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { builtin }

!llvm.ident = !{!0}

!0 = !{!"clang version 5.0.0 (git@github.com:cmtice/clang-experimental.git 3c6e6f19373f1bd0a6246346730fb82edbaa1c4a) (git@github.com:cmtice/llvm-experimental.git 3c29fb0ca228e8e466433a617d2fc5f7766ac692)"}
