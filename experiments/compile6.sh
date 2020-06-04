#!/bin/sh

nasm -f macho64 test6.s -o test6.o
ld -e _main -macosx_version_min 10.8 -arch x86_64 test6.o -lSystem -o test6
./test6