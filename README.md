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
4. Optimizer!
5. Implement real GC.
6. Abstract frame to different OS: AMD64 Linux, 64-bit Windows (why not), ARM something ʘ‿ʘ

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
cabal -v0 new-run PrintAssem  examples/hello_world.tiger > hello_world.s
cc hello_world.s runtime/build/libtiger_rt.a -lc++ -o hello_world
./hello_world
```

NB: I'm still working through the bugs :)
