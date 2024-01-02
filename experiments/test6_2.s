;
; assemble and link with:
; nasm -f elf test6_2.s && gcc -m32 test6_2.o -o test6_2
;
section .text
global _main
extern printf

_main:

	mov eax, 0xDEADBEEF
	push eax
	push message
	add esp, 8
	ret

message db "Register = %08X", 10, 0
