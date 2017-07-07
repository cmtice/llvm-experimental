; Test a program defined a local struct and has access on each field
;
; RUN: llvm-as < %s > %t1
; RUN: llvm-lto -O0 -struct-field-cache-analysis -o %t2 %t1 | FileCheck %s
; CHECK: There are 1 struct types are accessed in the program
; CHECK: Struct [struct.FooBar] defined as local struct has 4 accesses and 0 execution count.

%struct.FooBar = type <{ i32, i32, float, i8 }>

define void @foo() {
  %1 = alloca %struct.FooBar, align 8
  %2 = getelementptr inbounds %struct.FooBar, %struct.FooBar* %1, i64 0, i32 0
  %3 = load i32, i32* %2, align 8
  %4 = getelementptr inbounds %struct.FooBar, %struct.FooBar* %1, i64 0, i32 1
  %5 = load i32, i32* %4, align 8
  %6 = icmp slt i32 %3, %5
  br i1 %6, label %7, label %13

; <label>:7:                                      ; preds = %1
  %8 = getelementptr inbounds %struct.FooBar, %struct.FooBar* %1, i64 0, i32 2
  %9 = load float, float* %8, align 4
  br label %10

; <label>:10:                                     ; preds = %10, %7
  %11 = getelementptr inbounds %struct.FooBar, %struct.FooBar* %1, i64 0, i32 3
  %12 = load i8, i8* %11, align 8
  br label %10

; <label>:13:                                     ; preds = %1
  ret void
}