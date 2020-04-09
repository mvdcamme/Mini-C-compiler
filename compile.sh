#!/bin/sh

clang -O0 -mllvm --x86-asm-syntax=att test2.c -S
as -o test2.o test2.s
ld -macosx_version_min 10.8.0 -o test2 test2.o -lSystem
./test2