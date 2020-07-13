# HaskellTiger
Andrew Appel's _Modern Compiler Implementation in ML_, implemented in Haskell

Hobby project, work-in-progress: learning Haskell, while working through compiler book.

I have no idea what I'm doing :clown_face:

Currently targeting only MacOS on x86_64.
Linux System V x86_86 wouldn't be much of a change, nor would 64 bit Windows.

## Remaining pieces:

1. ~~Implement register allocation~~ (started Dec 2019, finished Feb 2020)
2. ~~Hook everything up~~
3. ~~Test for real, and fix the bugs! ☜(⌒▽⌒)☞~~ (started Feb, 2020, good enough progress at end of March 2020 -- eight queens and merge examples both work!)
4. Optimizer! Jul 2020: going to do a few more, then call it quits
5. LLVM backend!
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
.L15:
        .asciz  "hello world"
_main:
        push rbp                 ## ("_main",line -1, col -1)
        mov rbp, rsp
        sub rsp, 32
        lea rbx, [rip + .L15]
        mov qword ptr [rbp-8], rbx
        mov rbx, 11
        mov qword ptr [rbp-16], rbx
        lea r15, [rip + _tiger_allocString]
        mov r14, rax            ## caller saves
        mov r13, rdx            ## caller saves
        mov r12, rsi            ## caller saves
        mov rbx, rdi            ## caller saves
        mov rdi, qword ptr [rbp-8]
        mov rsi, qword ptr [rbp-16]
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
        add rsp, 32
        pop rbp
        mov rax, 0
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
