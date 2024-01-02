section .data       ; data segment
msg     db      "Hello, world!", 0x0a   ; the string and newline char

section .text       ; text segment
global main       ; Default entry point for ELF linking
extern printf

main:
	; SYSCALL: write(1, msg, 14)
	push msg
	call printf
	mov eax, 1        ; put 1 into eax, since exit is syscall #1
	mov ebx, 0        ; exit with success
	int 0x80          ; do the syscall
