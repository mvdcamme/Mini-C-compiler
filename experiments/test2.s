	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 13, 0	sdk_version 13, 1
	.globl	_main                           ## -- Begin function main
	.p2align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## %bb.0:
	pushl	%ebp
	.cfi_def_cfa_offset 8
	.cfi_offset %ebp, -8
	movl	%esp, %ebp
	.cfi_def_cfa_register %ebp
	pushl	%esi
	pushl	%eax
	.cfi_offset %esi, -12
	calll	L0$pb
L0$pb:
	popl	%esi
	subl	$40, %esp
	pushl	$2
	pushl	$1
	calll	_f
	addl	$40, %esp
	pushl	%eax
	pushl	$40
	calll	_add
	addl	$8, %esp
	leal	L_.str-L0$pb(%esi), %ecx
	pushl	%eax
	pushl	%ecx
	calll	_printf
	addl	$16, %esp
	xorl	%eax, %eax
	addl	$4, %esp
	popl	%esi
	popl	%ebp
	retl
	.cfi_endproc
                                        ## -- End function
	.globl	_add                            ## -- Begin function add
	.p2align	4, 0x90
_add:                                   ## @add
	.cfi_startproc
## %bb.0:
	pushl	%ebp
	.cfi_def_cfa_offset 8
	.cfi_offset %ebp, -8
	movl	%esp, %ebp
	.cfi_def_cfa_register %ebp
	movl	12(%ebp), %eax
	addl	8(%ebp), %eax
	popl	%ebp
	retl
	.cfi_endproc
                                        ## -- End function
	.globl	_f                              ## -- Begin function f
	.p2align	4, 0x90
_f:                                     ## @f
	.cfi_startproc
## %bb.0:
	pushl	%ebp
	.cfi_def_cfa_offset 8
	.cfi_offset %ebp, -8
	movl	%esp, %ebp
	.cfi_def_cfa_register %ebp
	movl	12(%ebp), %eax
	movl	8(%ebp), %ecx
	addl	%ecx, %eax
	addl	$3, %eax
	popl	%ebp
	retl
	.cfi_endproc
                                        ## -- End function
	.section	__TEXT,__cstring,cstring_literals
L_.str:                                 ## @.str
	.asciz	"%i\n"

.subsections_via_symbols
