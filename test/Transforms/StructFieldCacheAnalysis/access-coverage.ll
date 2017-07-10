; Test IR analysis coverage on types of accesses. 
; There are a load and a store on a GEP inst and a load and a store on GEP operator. All four instructions
; should be detected. The GEP result also passed to a function, which should be ignored and reported. It is
; also used in bitcast instruction, which should also be ignored and reported.
;
; RUN: llvm-as < %s > %t1
; RUN: llvm-lto -O0 -struct-field-cache-analysis -o %t2 %t1 | FileCheck %s

; CHECK: There are 1 struct types are accessed in the program
; CHECK: Struct [struct.FooBar] defined as global struct has 4 accesses.
; CHECK: Case GEP value passed into function calls was found 1 times
; CHECK: Case GEP value passed into bitcast was found 1 times


%struct.FooBar = type { i32, i8, double }

@global_foo = global %struct.FooBar zeroinitializer, align 8

; Function Attrs: noinline nounwind uwtable
define void @_Z4funcPd(double* %C) #0 {
entry:
  %C.addr = alloca double*, align 8
  store double* %C, double** %C.addr, align 8
  %0 = load double*, double** %C.addr, align 8
  store double 1.000000e+02, double* %0, align 8
  ret void
}

; Function Attrs: noinline norecurse nounwind uwtable
define i32 @main(i32 %argc, i8** %argv) #1 {
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
  call void @_Z4funcPd(double* getelementptr inbounds (%struct.FooBar, %struct.FooBar* @global_foo, i32 0, i32 2))
  ret i32 0
}