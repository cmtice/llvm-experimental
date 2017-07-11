; Test IR handling field address passed to functions.
; There are three function calls in the program. The first one is a traditional function calls
; and it should be detected. The second one is a call to an indirect function. Compiler should
; bail out and report in stats. The last one is a call to a function with unlimited arguments.
; Compiler should also bail out.
;
; RUN: llvm-as < %s > %t1
; RUN: llvm-lto -O0 -struct-field-cache-analysis -o %t2 %t1 | FileCheck %s
; CHECK: There are 1 struct types are accessed in the program
; CHECK: Struct [struct.FooBar] defined as global struct has 1 accesses and 0 execution count.
; CHECK: Case GEP value passed into indirect function calls or function that has undetermined num args was found 2 times


%struct.FooBar = type { i32, i8, double }

@global_foo = global %struct.FooBar zeroinitializer, align 8
@indirect_function = global void (i8*)* null, align 8
@.str = private unnamed_addr constant [3 x i8] c"%f\00", align 1

; Function Attrs: noinline nounwind uwtable
define void @_Z15normal_functionPi(i32* %A) #0 {
entry:
  %A.addr = alloca i32*, align 8
  store i32* %A, i32** %A.addr, align 8
  %0 = load i32*, i32** %A.addr, align 8
  store i32 10, i32* %0, align 4
  ret void
}

; Function Attrs: noinline nounwind uwtable
define void @_Z27indirect_function_candidatePc(i8* %B) #0 {
entry:
  %B.addr = alloca i8*, align 8
  store i8* %B, i8** %B.addr, align 8
  %0 = load i8*, i8** %B.addr, align 8
  store i8 65, i8* %0, align 1
  ret void
}

; Function Attrs: noinline nounwind uwtable
define void @_Z29function_with_undermined_argsPKcz(i8* %Format, ...) #0 {
entry:
  %Format.addr = alloca i8*, align 8
  store i8* %Format, i8** %Format.addr, align 8
  ret void
}

; Function Attrs: noinline norecurse uwtable
define i32 @main(i32 %argc, i8** %argv) #1 {
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
  call void %0(i8* getelementptr inbounds (%struct.FooBar, %struct.FooBar* @global_foo, i32 0, i32 1))
  call void (i8*, ...) @_Z29function_with_undermined_argsPKcz(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str, i32 0, i32 0), double* getelementptr inbounds (%struct.FooBar, %struct.FooBar* @global_foo, i32 0, i32 2))
  ret i32 0
}