#!/bin/sh

gcc -O0 -m32 test2.c -S
gcc -m32 test2.s -o test2.o -c
ld -macosx_version_min 10.14.0 -o test2 test2.o -lSystem
./test2
