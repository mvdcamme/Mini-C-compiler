; %include "stud_io.inc"    
global _main     

section .text
_main: 
    xor eax, eax
again:
    ; PRINT "Hello"
    ; PUTCHAR 10
    inc eax     
    cmp eax, 5
    jl again
