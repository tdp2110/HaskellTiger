# HaskellTiger
Andrew Appel's [_Modern Compiler Implementation in ML_](https://www.cs.princeton.edu/~appel/modern/ml/), implemented in Haskell.
Here is [a specification of the tiger language](https://www.lrde.epita.fr/~tiger/tiger.html).

Hobby project, work-in-progress: learning Haskell, while working through compiler book.

I have no idea what I'm doing :clown_face:

Currently targeting only MacOS on x86_64.
Linux System V x86_86 wouldn't be much of a change, nor would 64 bit Windows.

## Remaining pieces:

1. ~~Implement register allocation~~ (started Dec 2019, finished Feb 2020)
2. ~~Hook everything up~~
3. ~~Test for real, and fix the bugs! ☜(⌒▽⌒)☞~~ (started Feb, 2020, good enough progress at end of March 2020 -- eight queens and merge examples both work!)
4. ~~Optimizer! Jul 2020: going to do a few more, then call it quits~~
5. LLVM backend! Started Fall 2020.
6. Implement real GC.
7. Abstract frame to different OS: AMD64 Linux, 64-bit Windows (why not), ARM something ʘ‿ʘ

## Building

### Building runtime

The runtime, implemented in C++, is built with cmake:

```
cd runtime
mkdir build
cd build
cmake ..
cmake --build .
```

This builds a static library, libtiger_rt.a

### Building compiler

The compiler, implemented in Haskell, is built using cabal:

```
cabal new-build
```

## Compiling code:

For example, to compile and run examples/hello_world.tiger, after building the compiler and runtime

```
cabal -v0 new-run tigerc examples/hello_world.tiger > hello_world.s
clang hello_world.s runtime/build/libtiger_rt.a -lc++ -o hello_world
./hello_world
```

hello_world.tiger is simply

```
println("hello world")
```

hello_world.s will look something like

```
        .globl _main
        .section    __TEXT,__text,regular,pure_instructions
        .intel_syntax noprefix
.L16:
        .asciz  "hello world"
_main:                          ## (_main,line -1, col -1)
        push rbp
        mov rbp, rsp
        sub rsp, 16
        lea rbx, [rip + .L16]
        mov qword ptr [rbp-8], rbx
        lea r15, [rip + _tiger_allocString]
        mov r14, rax            ## caller saves
        mov r13, rdx            ## caller saves
        mov r12, rsi            ## caller saves
        mov rbx, rdi            ## caller saves
        mov rdi, qword ptr [rbp-8]
        mov rsi, 11
        call r15
        mov r15, rax
        mov rax, r14            ## caller restores
        mov rdx, r13            ## caller restores
        mov rsi, r12            ## caller restores
        mov rdi, rbx            ## caller restores
        lea r14, [rip + _tiger_println]
        mov r13, rax            ## caller saves
        mov r12, rdx            ## caller saves
        mov rbx, rdi            ## caller saves
        mov rdi, r15
        call r14
        mov rax, r13            ## caller restores
        mov rdx, r12            ## caller restores
        mov rdi, rbx            ## caller restores
        mov rax, rbp
        add rsp, 16
        pop rbp
        xor rax, rax
        ret
```

As you can see, it is not optimal :)

## Running tests

There are two sets of tests, first are unit test written in Haskell:

```
cabal new-test
```

and an integration test written in Python:

```
python3 test/compiler_test.py
```

(I should probably unify those to run from a single command :D)

## tigerc options

tigerc has a few options to show various stages of compilation.

```
✗ cabal -v0 new-run tigerc -- --help
Usage: tigerc [OPTION...] files...
      --show-tokens   tokenize input file
      --show-ast      parse input file to AST
      --show-tree-ir  show tree intermediate representation
      --show-flat-ir  show flattened intermediate representation
      --dump-cfg      dump dotgraph of control flow graphs (work in progress)
      --noreg         compile to asm without performing register allocation
      --O0            optimization level 0 (no optimization)
  -h  --help          show help
```

Let's explore a few using the hello_world.tiger example from above.
`--show-ast` pretty-prints the ast using the (awesome) [pretty-simple package](https://hackage.haskell.org/package/pretty-simple).

```
✗ cabal -v0 new-run tigerc -- examples/hello_world.tiger --show-ast
CallExp
    { func = println
    , args = [ StringExp "hello world" ]
    , pos = line 1
    , col 22
    }
```

`--show-tree-ir` shows the tree intermediate representation

```
✗ cabal -v0 new-run tigerc -- examples/hello_world.tiger --show-tree-ir
;; FRAG STRING:

(Label .L14,"hello world")
;; END FRAG
CALL(
  NAME _tiger_println,
  ESEQ(
    MOVE(
      TEMP 43,
      CALL(
        NAME _tiger_allocString,
        NAME .L14,
        CONST 11)),
    TEMP 43))
```

`--show-flat-ir` shows the flattend tree IR

```
✗ cabal -v0 new-run tigerc -- examples/hello_world.tiger --show-flat-ir
LABEL _main ## (_main,line -1, col -1)
MOVE(
  TEMP 44,
  CALL(
    NAME _tiger_allocString,
    NAME .L15,
    CONST 11))
MOVE(
  TEMP 45,
  CALL(
    NAME _tiger_println,
    TEMP 44))
MOVE(
  TEMP 0,
  TEMP 45)
JUMP(
  NAME .L16)
LABEL .L16
```

To see instruction selection before register allocation, use `--noreg`

```
    .globl _main
    .section    __TEXT,__text,regular,pure_instructions
    .intel_syntax noprefix
.L15:
    .asciz	"hello world"
_main:				## (_main,line -1, col -1)
    push rbp
    mov rbp, rsp
    sub rsp, 16
    lea t52, [rip + .L15]
    mov t53, 11
    lea t54, [rip + _tiger_allocString]
    mov t55, rax		## caller saves
    mov t56, rcx		## caller saves
    mov t57, rdx		## caller saves
    mov t58, rsi		## caller saves
    mov t59, rdi		## caller saves
    mov t60, r8		## caller saves
    mov t61, r9		## caller saves
    mov t62, r10		## caller saves
    mov t63, r11		## caller saves
    mov rdi, t52
    mov rsi, t53
    call t54
    mov t51, rax
    mov rax, t55		## caller restores
    mov rcx, t56		## caller restores
    mov rdx, t57		## caller restores
    mov rsi, t58		## caller restores
    mov rdi, t59		## caller restores
    mov r8, t60		## caller restores
    mov r9, t61		## caller restores
    mov r10, t62		## caller restores
    mov r11, t63		## caller restores
    mov t44, t51
    lea t65, [rip + _tiger_println]
    mov t66, rax		## caller saves
    mov t67, rcx		## caller saves
    mov t68, rdx		## caller saves
    mov t69, rsi		## caller saves
    mov t70, rdi		## caller saves
    mov t71, r8		## caller saves
    mov t72, r9		## caller saves
    mov t73, r10		## caller saves
    mov t74, r11		## caller saves
    mov rdi, t44
    call t65
    mov rax, t66		## caller restores
    mov rcx, t67		## caller restores
    mov rdx, t68		## caller restores
    mov rsi, t69		## caller restores
    mov rdi, t70		## caller restores
    mov r8, t71		## caller restores
    mov r9, t72		## caller restores
    mov r10, t73		## caller restores
    mov r11, t74		## caller restores
    mov t45, t64
    mov rax, t45
    add rsp, 16
    pop rbp
    xor rax, rax
    ret

```

## Optimizations

tigerc implements a few basic optimizations (all of them quite poorly implemented :D),
including constant propagation, constant folding, and various forms dead code elimination (but definitely not all!).
Some optimizations take place during AST walking and IR translation, others take place on the (tree) IR,
and others operate on the Assem IR. tigerc has an option `--O0` to turn off most optimizations.
For example, consider

```
✗ cat examples/constexpr-div.tiger
let
    var x := 2
    var y := 0
in
    println(itoa(x / y))
end
```

Compiling this tiger program with `--O0` gives

```
        .globl _main
        .section    __TEXT,__text,regular,pure_instructions
        .intel_syntax noprefix
_main:                          ## (_main,line -1, col -1)
        push rbp
        mov rbp, rsp
        sub rsp, 16
        mov rax, 2
        xor rbx, rbx
        cmp rbx, 0
        jne .L15
        jmp .L16
.L16:
        lea rax, [rip + _tiger_divByZero]
        call rax
.L15:
        cqo
        idiv rbx
        lea r14, [rip + _tiger_itoa]
        mov r13, rax            ## caller saves
        mov r12, rdx            ## caller saves
        mov rbx, rdi            ## caller saves
        mov rdi, rax
        call r14
        mov r15, rax
        mov rax, r13            ## caller restores
        mov rdx, r12            ## caller restores
        mov rdi, rbx            ## caller restores
        lea r14, [rip + _tiger_println]
        mov r13, rax            ## caller saves
        mov r12, rdx            ## caller saves
        mov rbx, rdi            ## caller saves
        mov rdi, r15
        call r14
        mov rax, r13            ## caller restores
        mov rdx, r12            ## caller restores
        mov rdi, rbx            ## caller restores
        xor rax, rax
        jmp .L17
.L17:
        add rsp, 16
        pop rbp
        xor rax, rax
        ret
```

That form checks the divisor (zero) against zero, and branches to a `[[noreturn]]`
failure function `_tiger_divByZero` if it matches (it will), else it would do the
division and print the result. tigerc can see that this will surely fail,
and when optimizations are turned on gives

```
✗ cabal -v0 new-run tigerc -- examples/constexpr-div.tiger
        .globl _main
        .section    __TEXT,__text,regular,pure_instructions
        .intel_syntax noprefix
_main:                          ## (_main,line -1, col -1)
        push rbp
        mov rbp, rsp
        lea rax, [rip + _tiger_divByZero]
        call rax
```

## LLVM Backend

Starting in Nov 2020, we've begun an LLVM backend using the *awesome*
[llvm-hs project](https://github.com/llvm-hs/llvm-hs). This is much less fully-featured than the native x86 backend.
As an example, consider examples/fibonacci.tiger:

```
$ cat examples/fibonacci.tiger
let
    function fib(n: int) : int =
        if n < 2 then
            n
        else
            fib(n - 1) + fib(n - 2)
in
    println(itoa(fib(25)))
end
```

We can compile this to LLVM IR with

```
$ cabal -v0 new-run tigerc examples/fibonacci.tiger -- --emit-llvm
; ModuleID = 'examples/fibonacci.tiger'





%string = type opaque


declare external ccc  void @tiger_divByZero()


declare external ccc  %string @tiger_itoa(i64)


declare external ccc  void @tiger_println(%string)


declare external ccc  void @tiger_print(%string)


define external ccc  i64 @fib(i64  %n_0)    {
entry_0:
  %0 = alloca i64, align 8
  store  i64 %n_0, i64* %0, align 8
  %1 = load  i64, i64* %0, align 8
  %2 = icmp slt i64 %1, 2
  %3 = icmp ne i1 %2, 0
  br i1 %3, label %if.then_0, label %if.else_0
if.then_0:
  %4 = load  i64, i64* %0, align 8
  br label %if.exit_0
if.else_0:
  %5 = load  i64, i64* %0, align 8
  %6 = sub   i64 %5, 1
  %7 =  call ccc  i64  @fib(i64  %6)
  %8 = load  i64, i64* %0, align 8
  %9 = sub   i64 %8, 2
  %10 =  call ccc  i64  @fib(i64  %9)
  %11 = add   i64 %7, %10
  br label %if.exit_0
if.exit_0:
  %12 = phi i64 [%4, %if.then_0], [%11, %if.else_0]
  ret i64 %12
}


define external ccc  i64 @main()    {
entry_0:
  %0 =  call ccc  i64  @fib(i64  25)
  %1 =  call ccc  %string  @tiger_itoa(i64  %0)
   call ccc  void  @tiger_println(%string  %1)
  ret i64 0
}
```

We can run the llvm optimizer on this IR by invoking `opt` (which may not be in your path):

```
$ cabal -v0 new-run tigerc examples/fibonacci.tiger -- --emit-llvm | opt -S -O2
; ModuleID = '<stdin>'
source_filename = "<stdin>"

%string = type opaque

declare %string @tiger_itoa(i64) local_unnamed_addr

declare void @tiger_println(%string) local_unnamed_addr

; Function Attrs: nounwind readnone
define i64 @fib(i64 %n_0) local_unnamed_addr #0 {
entry_0:
  %0 = icmp slt i64 %n_0, 2
  br i1 %0, label %if.exit_0, label %if.else_0

if.else_0:                                        ; preds = %entry_0
  %1 = add nsw i64 %n_0, -1
  %2 = tail call i64 @fib(i64 %1)
  %3 = add nsw i64 %n_0, -2
  %4 = tail call i64 @fib(i64 %3)
  %5 = add i64 %4, %2
  ret i64 %5

if.exit_0:                                        ; preds = %entry_0
  ret i64 %n_0
}

define i64 @main() local_unnamed_addr {
entry_0:
  %0 = tail call i64 @fib(i64 25)
  %1 = tail call %string @tiger_itoa(i64 %0)
  tail call void @tiger_println(%string zeroinitializer)
  ret i64 0
}

attributes #0 = { nounwind readnone }
```
