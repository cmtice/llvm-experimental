; Test IR analysis coverage on types of accesses. 
; There are a load and a store on a GEP inst and a load and a store on GEP operator. All four instructions
; should be detected. The GEP result also passed to a function, which will be detected as counted as the number
; of call/invoke instructions. GEP is also used in bitcast instruction, which should also be ignored and reported.
;
; RUN: llvm-as < %s > %t1
; RUN: llvm-lto -O0 -struct-field-cache-analysis -struct-analysis-IR-only -o %t2 %t1 | FileCheck %s

; CHECK: There are 1 struct types are accessed in the program
; CHECK: Struct [struct.FooBar] defined as global struct has 7 accesses and 0 execution count.


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
  store i64 10, i64* %3, align 8
  %4 = load i64, i64* %3, align 8
  call void @_Z4funcPd(double* getelementptr inbounds (%struct.FooBar, %struct.FooBar* @global_foo, i32 0, i32 2))
  ret i32 0
}