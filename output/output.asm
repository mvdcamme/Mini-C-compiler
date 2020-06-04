IDEAL
P386
MODEL FLAT, C
ASSUME cs:_TEXT,ds:FLAT,es:FLAT,fs:FLAT,gs:FLAT

STACK



DATASEG

	global_0 dw ?
	global_1 dw ?
	global_2 dw ?

CODESEG

PROC printInt
		push	ebp
		mov		ebp, esp
		pushad
		mov		eax, [ dword ebp + 8 ]
		mov		ecx, 0
		mov		ebx, 10
	doOneDigit:
		xor		edx, edx
		div		ebx
		push	edx
		inc		ecx
		cmp		eax, 0
		jnz		doOneDigit
	printOneDigit:
		pop		edx
		add		edx, 48
		mov		ah, 2
		int		21h
		dec		ecx
		jnz		printOneDigit
		mov		dl, 13
		mov		ah, 02h
		int		21h
		mov		dl, 10
		mov		ah, 02h
		int		21h
		popad
		mov		esp, ebp
		pop		ebp
		ret		
ENDP printInt

PROC g
		push	ebp
		mov		ebp, esp
		sub		esp, 2
		; MovCode (Literal (IntValue 5)) (OutAddr (Local 0 (Atom CharType)))
		mov		[ word ebp - 4 ], 5
		; Rt1Code (InAddr (Local 0 (Atom CharType)))
		mov		eax, [ word ebp - 4 ]
		mov		esp, ebp
		pop		ebp
		ret		
ENDP g
PROC f
		push	ebp
		mov		ebp, esp
		sub		esp, 21
		; MovCode (Literal (IntValue 10)) (OutAddr (Local 1 (Atom CharType)))
		mov		[ word ebp - 6 ], 10
		; CstCode (InAddr (Local 1 (Atom CharType))) (OutAddr (Global 2 (Atom IntType)))
		push	eax
		xor		eax, eax
		mov		ax, [ word ebp - 6 ]
		mov		[ offset global_2 ], eax
		pop		eax
		; MovCode (Literal (IntValue 1)) (OutAddr (Local 2 (Atom CharType)))
		mov		[ word ebp - 8 ], 1
		; CstCode (InAddr (Local 2 (Atom CharType))) (OutAddr (Parameter 0 (Atom IntType)))
		push	eax
		xor		eax, eax
		mov		ax, [ word ebp - 8 ]
		mov		[ dword ebp + 40 ], eax
		pop		eax
		; JzCode (InAddr (Parameter 0 (Atom IntType))) (Marker 0)
		cmp		[ dword ebp + 40 ], 0
		jz		label_0
		; ArgCode (InAddr (Parameter 0 (Atom IntType)))
		push	[ dword ebp + 40 ]
		; PrtCode 1
		call	printInt
		add		esp, 4
		; MovCode (Literal (IntValue 2)) (OutAddr (Local 4 (Atom CharType)))
		mov		[ word ebp - 10 ], 2
		; CstCode (InAddr (Local 4 (Atom CharType))) (OutAddr (Local 5 (Atom IntType)))
		push	eax
		xor		eax, eax
		mov		ax, [ word ebp - 10 ]
		mov		[ dword ebp - 12 ], eax
		pop		eax
		; AddCode (InAddr (Parameter 0 (Atom IntType))) (InAddr (Local 5 (Atom IntType))) (OutAddr (Local 6 (Atom IntType)))
		push	eax
		mov		eax, [ dword ebp + 40 ]
		add		eax, [ dword ebp - 12 ]
		mov		[ dword ebp - 16 ], eax
		pop		eax
		; Rt1Code (InAddr (Local 6 (Atom IntType)))
		mov		eax, [ dword ebp - 16 ]
		mov		esp, ebp
		pop		ebp
		ret		
		; JmpCode (Marker 1)
		jmp		label_1
		; LblCode (Marker 0)
	label_0:
		; MovCode (Literal (IntValue 1)) (OutAddr (Local 7 (Atom CharType)))
		mov		[ word ebp - 20 ], 1
		; Rt1Code (InAddr (Local 7 (Atom CharType)))
		mov		eax, [ word ebp - 20 ]
		mov		esp, ebp
		pop		ebp
		ret		
		; LblCode (Marker 1)
	label_1:
ENDP f
PROC main
		push	ebp
		mov		ebp, esp
		sub		esp, 19
		sti
		cld
		; MovCode (Literal (IntValue 1)) (OutAddr (Local 0 (Atom CharType)))
		mov		[ word ebp - 4 ], 1
		; CstCode (InAddr (Local 0 (Atom CharType))) (OutAddr (Local 1 (Atom IntType)))
		push	eax
		xor		eax, eax
		mov		ax, [ word ebp - 4 ]
		mov		[ dword ebp - 6 ], eax
		pop		eax
		; MovCode (Literal (IntValue 2)) (OutAddr (Local 2 (Atom CharType)))
		mov		[ word ebp - 8 ], 2
		; CstCode (InAddr (Local 2 (Atom CharType))) (OutAddr (Local 3 (Atom IntType)))
		push	eax
		xor		eax, eax
		mov		ax, [ word ebp - 8 ]
		mov		[ dword ebp - 22 ], eax
		pop		eax
		; ArgCode (InAddr (Local 3 (Atom IntType)))
		push	[ dword ebp - 22 ]
		; ArgCode (InAddr (Local 1 (Atom IntType)))
		push	[ dword ebp - 6 ]
		; CllCode "f" 2 (OutAddr (Local 4 (Atom IntType)))
		call	f
		add		esp, 8
		mov		[ dword ebp - 10 ], eax
		; ArgCode (InAddr (Local 4 (Atom IntType)))
		push	[ dword ebp - 10 ]
		; PrtCode 1
		call	printInt
		add		esp, 4
		; MovCode (Literal (IntValue 0)) (OutAddr (Local 6 (Atom CharType)))
		mov		[ word ebp - 16 ], 0
		; Rt1Code (InAddr (Local 6 (Atom CharType)))
		mov		eax, [ word ebp - 16 ]
		mov		esp, ebp
		pop		ebp
		ret		
ENDP main

end main