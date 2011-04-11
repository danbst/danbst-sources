;.MACRO
.equ ACELL = 1
.def temp = r16
.def tempL = r17
.def tempH = r18
.equ SPH = 0x3E
.equ SPL = 0x3D
.equ MAGIC_MEMORY_INIT = 0xC0
.equ MCUCR = 0x35

.equ SREG = 0x3F
.equ ZFLAG = 1

.def XH = r27
.def XL = r26
.def YH = r29
.def YL = r28
.def ZH = r31
.def ZL = r30

.def ENTRY_POINTH = r11
.def ENTRY_POINTL = r10
.eseg
.db 0
;.DATA
.dseg

; don't depart this block!!!
IP:	.byte 2
W:	.byte 2
; -----------
DSP: .byte 2

NEXT_WORD: .byte 2
ENTER_WORD: .byte 2
EXIT_WORD: .byte 2

DUP_WORD: .byte 2
DROP_WORD: .byte 2

DATA_STACK: .byte 50
RETURN_STACK: .byte 2

ENTRY_POINT: .byte 60
PROG_START: .byte 60
WORDS_START: .byte 2

.cseg
	; InitStack
	ldi temp, low(RETURN_STACK)
	out SPL, temp
	ldi temp, high(RETURN_STACK)
	out SPH, temp

	; InitExternalMemory
	ldi temp, MAGIC_MEMORY_INIT
	out	MCUCR, temp

	; Init Data Stack
	ldi XL, low(DSP)
	ldi XH, high(DSP)
	ldi tempL, low(DATA_STACK)
	ldi tempH, high(DATA_STACK)
	st X+, tempL
	st X+, tempH

	; load base program
	rcall copyFirmware

	; load PROG_START
	ldi XL, low(PROG_START)
	ldi XH, high(PROG_START)
	ldi tempL, low((mem_TEST2 - START_CODE)*2 + WORDS_START)
	ldi tempH, high((mem_TEST2 - START_CODE)*2 + WORDS_START)
	st X+, tempL
	st X+, tempH

	; load IP
	ldi XL, low(IP)
	ldi XH, high(IP)
	ldi tempL, low(PROG_START)
	ldi tempH, high(PROG_START)
	st X+, tempL
	st X+, tempH

	; jmp NEXT
	rjmp NEXT


_ENTRY_POINT:
	ldi ZL, 155
	ldi ZH, 0
	rcall ds_PUSH_Z
	ldi ZL, low(0xA006)
	ldi ZH, high(0xA006)
	rcall ds_PUSH_Z
	rcall _EXCLAM
	lop: rjmp lop

copyFirmware:
	;ldi tempL, low(END_CODE)
	;ldi tempH, high(END_CODE)
	;subi tempL, low(START_CODE)
	;sbci tempH, high(START_CODE)
	; now tempHL = len of code


	ldi ZL, low(START_CODE*2)
	ldi ZH, high(START_CODE*2)
	ldi XL, low(WORDS_START)
	ldi XH, high(WORDS_START)
start_loop:	
	lpm tempL, Z+
	lpm tempH, Z+
	st X+, tempL
	st X+, tempH

	; if (Z == END_CODE*2) goto start_loop;
	cpi ZL, low(END_CODE*2)
	in r0, SREG
	sbrs r0, ZFLAG
	rjmp start_loop
	cpi ZH, high(END_CODE*2)
	in r0, SREG
	sbrs r0, ZFLAG
	rjmp start_loop

	ret

; Word structure
; - Name (padded spaces left to 2n+1 bytes)
; - Name length (1 byte)
; - Link (2 bytes)
; - CFA (2 bytes)
; - rest

START_CODE:

mem_LIT:	.dw itc_LIT
mem_ENTER:	.dw ENTER
mem_EXIT:	.dw EXIT

			.db "+", 1
			.dw 0
mem_PLUS:	.dw itc_PLUS
			.db "@", 1
			.dw (mem_PLUS - START_CODE)*2 + WORDS_START
mem_AT:		.dw itc_AT
			.db " >R", 2
			.dw (mem_AT - START_CODE)*2 + WORDS_START
mem_TO_R:	.dw itc_TO_R
			.db " R>", 2
			.dw (mem_TO_R- START_CODE)*2 + WORDS_START
mem_FROM_R:	.dw itc_R_FROM
			.db "-", 1
			.dw (mem_FROM_R - START_CODE)*2 + WORDS_START
mem_MINUS:	.dw itc_MINUS
			.db "DUP", 3 
			.dw (mem_MINUS - START_CODE)*2 + WORDS_START
mem_DUP:	.dw itc_DUP
			.db " DROP", 4
			.dw (mem_DUP - START_CODE)*2 + WORDS_START
mem_DROP:	.dw itc_DROP
			.db "!", 1
			.dw (mem_DROP - START_CODE)*2 + WORDS_START
mem_EXCLAM:	.dw itc_EXCLAM

			.db " TEST", 4
			.dw (mem_EXCLAM - START_CODE)*2 + WORDS_START
mem_TEST:	.dw ENTER
			.dw (mem_LIT - START_CODE)*2 + WORDS_START
			.dw 1
			.dw (mem_LIT - START_CODE)*2 + WORDS_START
			.dw 0xA006
			.dw (mem_EXCLAM - START_CODE)*2 + WORDS_START
			.dw (mem_EXIT - START_CODE)*2 + WORDS_START

			.db "TEST2", 5
			.dw (mem_TEST - START_CODE)*2 + WORDS_START
mem_TEST2:	.dw ENTER

			.dw (mem_LIT - START_CODE)*2+WORDS_START
			.dw 8
			.dw (mem_AT - START_CODE)*2+WORDS_START
			.dw (mem_DUP - START_CODE)*2+WORDS_START
			.dw (mem_LIT - START_CODE)*2+WORDS_START
			.dw 1
			.dw (mem_PLUS - START_CODE)*2+WORDS_START
			.dw (mem_LIT - START_CODE)*2+WORDS_START
			.dw 8
			.dw (mem_EXCLAM - START_CODE)*2+WORDS_START
			.dw (mem_LIT - START_CODE)*2 + WORDS_START
			.dw 0xA004
			.dw (mem_EXCLAM - START_CODE)*2 + WORDS_START

			.dw (mem_FROM_R - START_CODE)*2 + WORDS_START
			.dw (mem_LIT - START_CODE)*2+WORDS_START
			.dw 2
			.dw (mem_MINUS - START_CODE)*2+WORDS_START
			.dw (mem_TO_R - START_CODE)*2 + WORDS_START
			
			.dw (mem_EXIT - START_CODE)*2 + WORDS_START

END_CODE:




NEXT:
	; (IP) -> W
	; IP += ACELL
	; jmp (W)
	
	ldi temp, low(IP)
	mov XL, temp
	ldi temp, high(IP)
	mov XH, temp
	; X = *IP
	ld ZL, X+
	ld ZH, X+
	; Z = IP, X = *(IP+2)
	ld YL, Z+	
	ld YH, Z+	
	; Y = (IP), Z = IP + CELL, X = *(IP+2) = *W
	ldi temp, low(IP)
	mov XL, temp
	ldi temp, high(IP)
	mov XH, temp
	; Y = (IP), Z = IP + CELL, X = *IP
	st X+, ZL
	st X+, ZH
	; Y = (IP), X = *(IP+2), IP = IP + CELL
	st X+, YL
	st X+, YH
	; Y = (IP), X = *(IP+4), IP = IP + CELL, W = (IP)
	ld ZL, Y+
	ld ZH, Y+
	; Y = (IP)+2, X = *(IP+4), Z = (Y) = (W), IP = IP + CELL, W = (IP)
	ijmp

ENTER:
	; push IP
	; W + ACELL -> IP
	; jmp NEXT
	ldi temp, low(IP)
	mov XL, temp
	ldi temp, high(IP)
	mov XH, temp
	ld r18, X+
	ld r19, X+
	push r19
	push r18

	ldi temp, low(W)
	mov XL, temp
	ldi temp, high(W)
	mov XH, temp
	ld YL, X+
	ld YH, X+
	ld XL, Y+
	ld XH, Y+
	; now Y = W + ACELL and what is in X it doesn't matter	
	ldi temp, low(IP)
	mov XL, temp
	ldi temp, high(IP)
	mov XH, temp
	st X+, YL
	st X+, YH

	rjmp NEXT

EXIT:
	; pop IP
	; jmp NEXT
	ldi temp, low(IP)
	mov XL, temp
	ldi temp, high(IP)
	mov XH, temp
	pop temp
	st X+, temp
	pop temp
	st X+, temp
	
	rjmp NEXT


ds_PUSH_Z:
	ldi temp, low(DSP)
	mov XL, temp
	ldi temp, high(DSP)
	mov XH, temp
	ld YL, X+
	ld YH, X+
	st Y+, ZL
	st Y+, ZH

	ldi temp, low(DSP)
	mov XL, temp
	ldi temp, high(DSP)
	mov XH, temp
	st X+, YL
	st X+, YH

	ret

ds_POP_Z:
	ldi temp, low(DSP)
	mov XL, temp
	ldi temp, high(DSP)
	mov XH, temp
	ld YL, X+
	ld YH, X+
	ld ZL, -Y
	ld ZH, -Y

	ldi temp, low(DSP)
	mov XL, temp
	ldi temp, high(DSP)
	mov XH, temp
	st X+, YL
	st X+, YH

	ld ZL, Y+
	ld ZH, Y+
	ret

itc_DUP:
	rcall _DUP
	rjmp NEXT
_DUP:
	nop
	rcall ds_POP_Z
	rcall ds_PUSH_Z
	rcall ds_PUSH_Z
	ret

itc_DROP:
	rcall _DROP
	rjmp NEXT
_DROP:
	nop
	rcall ds_POP_Z
	ret

itc_OVER:
	rcall _OVER
	rjmp NEXT
_OVER:
	nop
	rcall ds_POP_Z
	push ZH
	push ZL
	rcall _DUP
	pop ZL
	pop ZH
	rcall ds_PUSH_Z
	rcall _SWAP
	ret

itc_SWAP:
	rcall _SWAP
	rjmp NEXT
_SWAP:
	nop
	rcall ds_POP_Z
	push ZL
	push ZH
	rcall ds_POP_Z
	mov tempH, ZH
	mov tempL, ZL
	pop ZH
	pop ZL
	rcall ds_PUSH_Z
	mov ZH, tempH
	mov ZL, tempL
	rcall ds_PUSH_Z
	ret

itc_ROT:
	rcall _ROT
	rjmp NEXT
_ROT:
	nop
	rcall _SWAP
	pop ZH
	pop ZL
	rcall ds_PUSH_Z
	rcall _SWAP
	ret

itc_TO_R:
	rcall _TO_R
	rjmp NEXT
_TO_R:
	nop
	rcall ds_POP_Z
	pop tempH
	pop tempL
	push ZH
	push ZL
	push tempL
	push tempH
	ret

itc_R_FROM:
	rcall _R_FROM
	rjmp NEXT
_R_FROM:
	nop
	pop tempH
	pop tempL
	pop ZL
	pop ZH
	push tempL
	push tempH
	rcall ds_PUSH_Z
	ret

itc_R_AT:
	rcall _R_AT
	rjmp NEXT
_R_AT:
	nop
	pop tempH
	pop tempL
	pop ZL
	pop ZH
	push tempL
	push tempH
	rcall ds_PUSH_Z
	push ZH
	push ZL
	ret

itc_EXCLAM:
	rcall _EXCLAM
	rjmp NEXT
_EXCLAM:
	nop
	rcall _TO_R
	rcall ds_POP_Z
	mov tempH, ZH
	mov tempL, ZL
	pop ZL
	pop ZH
	st Z+, tempL
	st Z+, tempH
	ret

itc_AT:
	rcall _AT
	rjmp NEXT
_AT:
	nop
	rcall ds_POP_Z
	ld tempL, Z+
	ld tempH, Z+
	mov ZL, tempL
	mov ZH, tempH
	rcall ds_PUSH_Z
	ret

itc_LIT:
	rcall _LIT
	rjmp NEXT
_LIT:
	nop
	ldi temp, low(IP)
	mov XL, temp
	ldi temp, high(IP)
	mov XH, temp
	ld YL, X+
	ld YH, X+
;	ld ZL, Y+
;	ld ZH, Y+
	; now Y = IP + ACELL and what is in X it doesn't matter	
	ld ZL, Y+
	ld ZH, Y+
	; Z = number
	
	push YH
	push YL
	rcall ds_PUSH_Z
	pop YL
	pop YH

	ldi temp, low(IP)
	mov XL, temp
	ldi temp, high(IP)
	mov XH, temp
	st X+, YL	
	st X+, YH
	ret

itc_PLUS:
	rcall _PLUS
	rjmp NEXT
_PLUS:
	nop
	rcall ds_POP_Z
	mov tempL, ZL
	mov tempH, ZH
	rcall ds_POP_Z
	add ZL, tempL
	adc ZH, tempH
	rcall ds_PUSH_Z
	ret

itc_MINUS:
	rcall _MINUS
	rjmp NEXT
_MINUS:
	nop
	rcall ds_POP_Z
	mov tempL, ZL
	mov tempH, ZH
	rcall ds_POP_Z
	sub ZL, tempL
	sbc ZH, tempH
	rcall ds_PUSH_Z
	ret

itc_AND:
	rcall _AND
	rjmp NEXT
_AND:
	nop
	rcall ds_POP_Z
	mov tempL, ZL
	mov tempH, ZH
	rcall ds_POP_Z
	and ZL, tempL
	and ZH, tempH
	rcall ds_PUSH_Z
	ret

itc_OR:
	rcall _OR
	rjmp NEXT
_OR:
	nop
	rcall ds_POP_Z
	mov tempL, ZL
	mov tempH, ZH
	rcall ds_POP_Z
	or ZL, tempL
	or ZH, tempH
	rcall ds_PUSH_Z
	ret

itc_XOR:
	rcall _XOR
	rjmp NEXT
_XOR:
	nop
	rcall ds_POP_Z
	mov tempL, ZL
	mov tempH, ZH
	rcall ds_POP_Z
	eor ZL, tempL
	eor ZH, tempH
	rcall ds_PUSH_Z
	ret

itc_NEG:
	rcall _NEG
	rjmp NEXT
_NEG:
	nop
	rcall _INV
	ldi ZL, 1
	clr ZH
	rcall ds_PUSH_Z
	rcall _PLUS
	ret

itc_INV:
	rcall _INV
	rjmp NEXT
_INV:
	nop
	rcall ds_POP_Z
	com ZL
	com ZH
	rcall ds_PUSH_Z
	ret
