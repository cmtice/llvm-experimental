; Test a simple struct access can be detected
;
; RUN: llvm-as < %s > %t1
; RUN: llvm-lto -O0 -struct-field-cache-analysis -o %t2 %t1 | FileCheck %s
; CHECK: 