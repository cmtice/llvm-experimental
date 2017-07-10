; Test the pass is enabled and dummy stats printed out
;
; RUN: llvm-as < %s > %t1
; RUN: llvm-lto -struct-field-cache-analysis -o %t2 %t1 | FileCheck %s
; CHECK: Printing stats for struct accesses

define void @foo() {
  ret void
}