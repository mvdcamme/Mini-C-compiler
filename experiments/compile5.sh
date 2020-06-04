#!/bin/sh

nasm -f macho64 test5.s -o test5.o && ld -e _main -macosx_version_min 10.8 -arch x86_64 test5.o -lSystem -o test5 && ./test5
