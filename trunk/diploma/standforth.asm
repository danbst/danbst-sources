;################################################
;; GLOBAL CONFIG
;;
;>python
## Compiler type. Current available modes:
## - "avra" - linux avr assembler
## - "avrstudio" - windows AVRstudio4 assembler
Compiler = "avra"

## Tab size used in this document
TabSize = 4
;>endpy

;;===============================================
;; PART DEFINITION
;>python
if Compiler == "avra":
	pasteLine('.include "partdef.inc"')
elif Compiler == "avrstudio":
	pasteLine('.include "m8515def.inc"')
;>endpy

;;===============================================
;; REGISTER MAP
;.def dspL = r2 ; data stack pointer
;.def dspH = r3

.def destL = r4
.def destH = r5
.def sourceL = r6
.def sourceH = r7
.def countL = r8
.def countH = r9

.def ZZL = r20 ; акумулятор
.def ZZH = r21

.def tempIL = r10
.def tempIH = r11

.def temp	= r16
.def temp2	= r17
.def tempL	= r18
.def tempH	= r19
.def temp2L = r22
.def temp2H = r23

; free
; .def r0 =  ; використовується у операціях множення
; .def r1 =
; .def r12 =
; .def r13 =
; .def r14 =
; .def r15 =
 .def dspL = r24
 .def dspH = r25


;################################################
;; DATA SEGMENT
.dseg

DATA_STACK:		.byte 50
RETURN_STACK:	.byte 2

ENTRY_POINT:	.byte 60
PROG_START:		.byte 60
WORDS_START:	.byte 2

; Environment specific defs
.include "ev8031.asm"


;################################################
;; CHIP settings
; Define baud rate
;.equ USART_BAUD = 38400
.equ USART_UBBR_VALUE = 47

;################################################
;; MACRO and FUNCTIONS
;>python
# out IOregister, imm
addMacro("out", """
	ldi temp, @1
	out @0, temp
""")

# `store ADDR, imm16
addMacro("store","""
	ldi tempL, low(@1)
	ldi tempH, high(@1)
	sts @0,   tempL
	sts @0+1, tempH
""")

## "rammap" converts address in flash to address in RAM after
##   loading program to RAM
addFunc("rammap", "(@0 - START_CODE)*2 + WORDS_START")

## `ldiw temp, imm16
# temp - register (r16-r30, only even, may be X Y Z)
addMacro("ldiw", """
	ldi @0L, low(@1)
	ldi @0H, high(@1)
""")
;>endpy


;################################################
;; CODE SEGMENT
.cseg

	rjmp RESET

.org 0x40
RESET:
	; Ініціалізація стеку повернень
	`out SPL, low(RETURN_STACK)
	`out SPH, high(RETURN_STACK)

	; Дозволити зовнішню пам’ять
	;  Заодно налаштувати зовнішні переривання
	`out MCUCR, MAGIC_MEMORY_INIT; | (1<<ISC01) | (0<<ISC00)


	; Копіювати прошивку з flash у RAM
	rcall copyFirmware

	; Дозволити переривання
	;`out SREG, 1<<INT_ENABLE  ;? замінити на sei?
	;`out GICR, 1<<INT0
	rcall coldStart
	;`store #rammap(TIB),        #rammap(FORTH_PROG_START)
	`store #rammap(TIB), 0x4000
	`store #rammap(CTIB),       (FORTH_PROG_END - FORTH_PROG_START)*2

	;;;;;;;;;; UAARTTTT
UART_WAIT:
	rcall USART_vInit
	clr r15
	rcall USART_Receive

	`ldiw Z, 0x4000
uart_loop:
	rcall USART_Receive
	sts LEDs, temp
	sts INDL, temp
	inc r14
	cpi temp, 13
	  breq need_interpret
	st Z+, temp
	rjmp uart_loop

need_interpret:
	rcall USART_Flush
	subi ZL, low(0x4000)
	sbci ZH, high(0x4000)
	sts #rammap(CTIB), ZL
	sts #rammap(CTIB)+1, ZH
	`store #rammap(_IN),        0 ; Встановити курсор на 0 символ текстового буферу
	; Запустити асемблерний інтепретатор.

	`out UCSRC, (1<<URSEL)
	`out UCSRB, 0

	rjmp interpret

USART_Flush:
	sbis UCSRA, RXC
	ret
	in temp, UDR
	rjmp USART_Flush


USART_vInit:
	`out UBRRH, high(USART_UBBR_VALUE)
	`out UBRRL, low(USART_UBBR_VALUE)

	`out UCSRC, (1<<URSEL) | (3<<UCSZ0)
	`out UCSRB, (1<<RXEN) | (1<<TXEN)
ret

USART_Receive:
	; Wait for data to be received
	sbis UCSRA, RXC
	rjmp USART_Receive
	; Get and return received data from buffer
	in temp, UDR
	ret

USART_Transmit:
	; Wait for empty transmit buffer
	sbis UCSRA,UDRE
	rjmp USART_Transmit
	; Put data (r16) into buffer, sends the data
	out UDR, temp
	ret

USART_SendACK:
	ldi temp, 0x33
	rcall USART_Transmit
	ldi temp, 0x33
	rcall USART_Transmit
	ldi temp, 0x33
	rcall USART_Transmit
	ret

copyFirmware:
	`ldiw Z, START_CODE * 2
	`ldiw X, WORDS_START
	`ldiw Y, END_CODE * 2
	sub YL, ZL ; Y = END_CODE - START_CODE
	sbc YH, ZH
	ldi temp, 0
	  start_loop:
		lpm tempL, Z+
		lpm tempH, Z+
		st X+, tempL
		st X+, tempH

		sbiw YL, 2
		cp YL, temp
		cpc YH, temp
		  brne start_loop
	ret


coldStart:
	; Ініціалізація стеку даних (в регістрах dspH:dspL)
	`ldiw dsp, DATA_STACK

	; Заповнити системні змінні Форту
	`store #rammap(_CP),        #rammap(END_CODE)
	`store #rammap(LATEST),     #rammap(mem_NOOP) ; переробити через lateVar
	`store #rammap(BASE),       10
	ret

;;;;;;;;>python
lastCFA = ""

def toName(w):
	return w[::-1][1:-1]

## `HEADER "w", faddr, naddr
# w - word name
# faddr - assembler code address
# naddr - generated memory CFA address
addMacro("HEADER", lambda w, faddr, naddr: True and
	pasteMacro("""
				.db "{0}", {1}
				.dw {2}
	{3}:		.dw {4}
	""".format(
				iif(len(w)%2 == 0," ", "") + toName(w), len(toName(w)),
				iif(lastCFA=="","0",FUNCS["rammap"](lastCFA)),
	naddr,		faddr
				), True, False) and
	gEX("lastCFA = '{0}'".format(naddr)) and
	addCompiledWord(eval(w), naddr)
)
addMacro("HEADER_IMM", lambda w, faddr, naddr: True and
	pasteMacro("""
				.db "{0}", {1}
				.dw {2}
	{3}:		.dw {4}
	""".format(
				iif(len(w)%2 == 0," ", "") + toName(w), len(toName(w))+128,
				iif(lastCFA=="","0",FUNCS["rammap"](lastCFA)),
	naddr,		faddr
				), True, False) and
	gEX("lastCFA = '{0}'".format(naddr)) and
	addCompiledWord(eval(w), naddr)
)

addMacro("COMPILE_RAW", lambda text: True and
	pasteMacro("\n".join(["\t\t.dw {0}".format(iif(wordDictHasKey(x),lambda:FUNCS["rammap"](wordDictF(x)),lambda:x)()) for x in eval(text).split()]), True, True)
)

addMacro("load","""
	lds @0L, @1
	lds @0H, @1+1
""")
;;;;;;;;;>endpy


; Word structure
; - Name (padded spaces left to 2n+1 bytes)
; - Name length (1 byte)
; - Link (2 bytes)
; - CFA (2 bytes)
; - rest

START_CODE:

;FORTH_PROG_START: .db "   TEST2 0 >IN !  "

mem_LIT:	.dw itc_LIT
mem_ENTER:	.dw ENTER
mem_EXIT:	.dw EXIT
mem_DOVAR:	.dw DOVAR
;>python
addCompiledWord("LIT", "mem_LIT")
addCompiledWord("ENTER", "mem_ENTER")
addCompiledWord("EXIT", "mem_EXIT")
addCompiledWord("DOVAR", "mem_DOVAR")
;>endpy

; Використовується для виклику повернення у асемблерний інтерпретатор
mem_RET:	.dw gethere
mmRET:		.dw #rammap(mem_RET)

;;;;;; АСЕМБЛЕРНІ СЛОВА!!! ;;;;;;;;
			`HEADER "+",     itc_PLUS,    mem_PLUS
			`HEADER "-",     itc_MINUS,   mem_MINUS
			`HEADER ">R",    itc_TO_R,    mem_TO_R
			`HEADER "R@",    itc_R_FETCH,    mem_TO_FETCH
			`HEADER "R>",    itc_R_FROM,  mem_R_FROM
			`HEADER "DUP",   itc_DUP,     mem_DUP
			`HEADER "DROP",  itc_DROP,    mem_DROP
			`HEADER "SWAP",  itc_SWAP,    mem_SWAP
			`HEADER "OVER",  itc_OVER,    mem_OVER
			`HEADER "@",     itc_AT,      mem_AT
			`HEADER "!",     itc_EXCLAM,  mem_EXCLAM
			`HEADER "C!",    itc_C_EXCLAM, mem_C_EXCLAM
			`HEADER "C@",    itc_C_AT,     mem_C_AT
			`HEADER "2*",    itc_2STAR,    mem_2STAR
			`HEADER "2/",    itc_2SLASH,   mem_2SLASH
			`HEADER "0",     itc_0,        mem_0
			`HEADER "-1",    itc_FFFF,     mem_FFFF
			`HEADER "CSWAP", itc_CSWAP,    mem_CSWAP
			`HEADER "AND",   itc_AND,     mem_AND
			`HEADER "XOR",   itc_XOR,     mem_XOR
			`HEADER "OR",    itc_OR,      mem_OR
			`HEADER "INVERT", itc_INVERT, mem_INVERT
			`HEADER "=",      itc_EQUAL,  mem_EQUAL
			`HEADER "HERE2",  itc_HERE,  mem_HERE2
			`HEADER "COLD",   itc_COLD,  mem_COLD

			`HEADER "TOKEN",   itc_TOKEN,      mem_TOKEN
			`HEADER "REVERSE", itc_REVERSESTR, mem_REVERSESTR
			`HEADER ">NUMBER", itc_NUMBER,     mem_NUMBER


;;;;;;; Змінні ;;;;;;;;
_CP:	.dw 0
		`HEADER "TIB", DOCONST, mem_TIB
TIB:	.dw 0
		`HEADER "#TIB", DOCONST, mem_CTIB
CTIB:	.dw 0
		`HEADER ">IN", DOCONST, mem__IN
_IN:	.dw 0
		`HEADER "BASE", DOCONST, mem_BASE
BASE:	.dw 10
		`HEADER "LATEST", DOCONST, mem_LATEST
LATEST:	.dw 0
		`HEADER "STATE", DOCONST, mem_STATE
STATE:	.dw 0

;;;;;; ФОРТ-слова!!! ;;;;;;;;;;;

		`HEADER "->LEDS", ENTER, mem_LEDS
		`COMPILE_RAW "LIT"
		.dw LEDs
		`COMPILE_RAW "C! EXIT"

		`HEADER "->IND", ENTER, mem_IND
		`COMPILE_RAW "CSWAP LIT"
		.dw INDICATOR
		`COMPILE_RAW "! EXIT"

		`HEADER "->INDL", ENTER, mem_INDL
		`COMPILE_RAW "LIT"
		.dw INDL
		`COMPILE_RAW "C! EXIT"

		`HEADER "->INDH", ENTER, mem_INDH
		`COMPILE_RAW "LIT"
		.dw INDH
		`COMPILE_RAW "C! EXIT"

		`HEADER "->INDDP", ENTER, mem_INDDP
		`COMPILE_RAW "LIT"
		.dw INDDP
		`COMPILE_RAW "C! EXIT"

		`HEADER "->portA", ENTER, mem_PORTA
		`COMPILE_RAW "LIT"
		.dw PA_REG
		`COMPILE_RAW "C! EXIT"
		`HEADER "->portB", ENTER, mem_PORTB
		`COMPILE_RAW "LIT"
		.dw PB_REG
		`COMPILE_RAW "C! EXIT"
		`HEADER "->portC", ENTER, mem_PORTC
		`COMPILE_RAW "LIT"
		.dw PC_REG
		`COMPILE_RAW "C! EXIT"

		`HEADER "1", ENTER, mem_1
		`COMPILE_RAW "0 -1 - EXIT"

		`HEADER "NEGATE", ENTER, mem_NEGATE
		`COMPILE_RAW "INVERT 1 + EXIT"

		`HEADER "CELL", ENTER, mem_CELL
		`COMPILE_RAW "LIT 2 EXIT"

		`HEADER "CELL+", ENTER, mem_CELLPLUS
		`COMPILE_RAW "CELL + EXIT"

		`HEADER "CELLS", ENTER, mem_CELLS
		`COMPILE_RAW "2* EXIT"

		`HEADER "HERE", ENTER, mem_HERE
		`COMPILE_RAW "LIT "
		.dw #rammap(_CP)
		`COMPILE_RAW "@ EXIT"

		`HEADER "ALLOT", ENTER, mem_ALLOT
		`COMPILE_RAW "HERE + LIT"
		.dw #rammap(_CP)
		`COMPILE_RAW "! EXIT"

		`HEADER ",", ENTER, mem_COMMA
		`COMPILE_RAW "HERE ! CELL ALLOT EXIT"

		`HEADER "ROT", ENTER, mem_ROT
		`COMPILE_RAW ">R SWAP R> SWAP EXIT"

		`HEADER_IMM "[", ENTER, mem_LEFTBRACKET ; interpret
		`COMPILE_RAW "0 STATE ! EXIT"

		`HEADER "]", ENTER, mem_RIGHTBRACKET ; compile
		`COMPILE_RAW "1 STATE ! EXIT"

		`HEADER "(CREATE)", ENTER, mem_CREATE
		`COMPILE_RAW " TOKEN HERE REVERSE CELLS ALLOT "
		`COMPILE_RAW " LATEST @ , HERE LATEST ! EXIT"

		`HEADER "CONSTANT", ENTER, mem_CONSTANT
		`COMPILE_RAW "(CREATE) LIT "
		.dw DOVAR
		 `COMPILE_RAW ", , EXIT"

		`HEADER "VARIABLE", ENTER, mem_VARIABLE
		`COMPILE_RAW "(CREATE) LIT "
		.dw DOCONST
		`COMPILE_RAW ", LIT 2 ALLOT EXIT"

		`HEADER ":", ENTER, mem_COLON
		`COMPILE_RAW " TOKEN HERE REVERSE CELLS ALLOT "
		`COMPILE_RAW " LATEST @ , HERE LATEST ! "
		`COMPILE_RAW " LIT "
		.dw ENTER
		`COMPILE_RAW " , ] EXIT "

		`HEADER ":NONAME", ENTER, mem_NONAME
		`COMPILE_RAW " HERE LIT "
		.dw ENTER
		`COMPILE_RAW " , ] EXIT "

		`HEADER_IMM ";", ENTER, mem_SEMICOLON
		`COMPILE_RAW " [ LIT EXIT , EXIT"
		.dw #rammap(mem_EXIT)
		`COMPILE_RAW ", EXIT "

		`HEADER_IMM "LITERAL", ENTER, mem_LITERAL
		`COMPILE_RAW " LIT LIT , , EXIT "















		`HEADER "NOOP", ENTER, mem_NOOP
		`COMPILE_RAW "EXIT"



FORTH_PROG_START:
;	.db " 1              "
;	.db " DUP LEDS C! 2* "
;	.db " DUP LEDS C! 2* "
;	.db " DUP LEDS C! 2* "
;	.db " DUP LEDS C! 2* "
;	.db " DUP LEDS C! 2* "
;	.db " DUP LEDS C! 2* "
;	.db " DUP LEDS C! 2* "
;	.db " DUP LEDS C! 2* "
;	.db " BASE @ "
;	.db "   DUP >INDL "
;	.db "   1 2* 2* BASE ! "
;	.db "   BASE @ >INDH "
;	.db " BASE !"
;	.db " 0 >IN  !    "

FORTH_PROG_END:;   .db "  ", 0 , 0



END_CODE:

;;;;;;; CORE ;;;;;;;
; IP     - XH:XL
; W+2    - YH:YL
; DSP    - dspH:dspL

NEXT:
	ld YL, X+
	ld YH, X+
	ld ZL, Y+
	ld ZH, Y+
	ijmp

ENTER:
	push XL
	push XH
	movw XL, YL
	rjmp NEXT

EXIT:
	pop XH
	pop XL
	rjmp NEXT

mRET:
	ret

ds_PUSH_ZZ:
	movw ZL, dspL
	st Z+, ZZL
	st Z+, ZZH
	movw dspL, ZL
	ret

ds_POP_ZZ:
	movw ZL, dspL
	ld ZZH, -Z
	ld ZZL, -Z
	movw dspL, ZL
	ret

DOVAR:
	movw ZL, YL
	ld ZZL, Z+
	ld ZZH, Z+
	rcall ds_PUSH_ZZ
	rjmp NEXT

DOCONST:
	movw ZZL, YL
	rcall ds_PUSH_ZZ
	rjmp NEXT

;;;;;;;;;;;;;;;;;;;;





itc_COLD:
	rcall coldStart

	; Ініціалізація стеку повернень
	`out SPL, low(RETURN_STACK)
	`out SPH, high(RETURN_STACK)

	rjmp interpret

itc_DUP:
	rcall _DUP
	rjmp NEXT
_DUP:
	movw ZL, dspL
	ld ZZH, -Z
	ld ZZL, -Z
	adiw ZL, 2
	st Z+, ZZL
	st Z+, ZZH
	movw dspL, ZL
	ret

itc_DROP:
	rcall _DROP
	rjmp NEXT
_DROP:
	movw ZL, dspL
	subi ZL, 2
	sbci ZH, 0
	movw dspL, ZL
	ret

itc_OVER:
	rcall _OVER
	rjmp NEXT
_OVER:
	movw ZL, dspL
	subi ZL, 4
	sbci ZH, 0
	ld ZZH, Z+
	ld ZZL, Z+
	movw ZL, dspL
	st Z+, ZZL
	st Z+, ZZH
	movw dspL, ZL
	ret

itc_SWAP:
	rcall _SWAP
	rjmp NEXT
_SWAP:
	movw ZL, dspL
	ld tempH, -Z
	ld tempL, -Z
	ld ZZH, -Z
	ld ZZL, -Z
	st Z+, tempL
	st Z+, tempH
	st Z+, ZZL
	st Z+, ZZH
	movw dspL, ZL
	ret

itc_CSWAP:
	rcall _CSWAP
	rjmp NEXT
_CSWAP:
	movw ZL, dspL
	ld tempH, -Z
	ld tempL, -Z
	st Z+, tempH
	st Z+, tempL
	movw dspL, ZL
	ret

itc_ROT:
	rcall _ROT
	rjmp NEXT
_ROT:
	rcall _SWAP
	pop ZZH
	pop ZZL
	rcall ds_PUSH_ZZ
	rcall _SWAP
	ret

itc_R_FETCH:
	pop  ZZH
	pop  ZZL
	push ZZL
	push ZZH
	rcall ds_PUSH_ZZ
	rjmp NEXT


itc_TO_R:
	rcall ds_POP_ZZ
	push ZZL
	push ZZH
	rjmp NEXT
_TO_R:
	rcall ds_POP_ZZ
	pop tempH
	pop tempL
	push ZZH
	push ZZL
	push tempL
	push tempH
	ret

itc_R_FROM:
	pop ZZH
	pop ZZL
	rcall ds_PUSH_ZZ
	rjmp NEXT
_R_FROM:
	pop tempH
	pop tempL
	pop ZZL
	pop ZZH
	push tempL
	push tempH
	rcall ds_PUSH_ZZ
	ret


itc_EXCLAM:
	rcall _EXCLAM
	rjmp NEXT
_EXCLAM:
	movw tempL, XL
	movw XL, dspL
	ld ZH, -X
	ld ZL, -X
	ld ZZH, -X
	ld ZZL, -X
	st Z+, ZZL
	st Z+, ZZH
	movw dspL, XL
	movw XL, tempL
	ret

itc_AT:
	rcall _AT
	rjmp NEXT
_AT:
	rcall ds_POP_ZZ
	movw ZL, ZZL
	ld ZZL, Z+
	ld ZZH, Z+
	rcall ds_PUSH_ZZ
	ret

itc_LIT:
	rcall _LIT
	rjmp NEXT
_LIT:
	ld ZZL, X+
	ld ZZH, X+
	rcall ds_PUSH_ZZ
	ret

itc_PLUS:
	rcall _PLUS
	rjmp NEXT
_PLUS:
	rcall ds_POP_ZZ
	mov temp2L, ZZL
	mov temp2H, ZZH
	rcall ds_POP_ZZ
	add ZZL, temp2L
	adc ZZH, temp2H
	rcall ds_PUSH_ZZ

	ret

itc_MINUS:
	rcall _MINUS
	rjmp NEXT
_MINUS:
	rcall ds_POP_ZZ
	mov temp2L, ZZL
	mov temp2H, ZZH
	rcall ds_POP_ZZ
	sub ZZL, temp2L
	sbc ZZH, temp2H
	rcall ds_PUSH_ZZ
	ret

itc_AND:
	rcall _AND
	rjmp NEXT
_AND:
	rcall ds_POP_ZZ
	mov temp2L, ZZL
	mov temp2H, ZZH
	rcall ds_POP_ZZ
	and ZZL, temp2L
	and ZZH, temp2H
	rcall ds_PUSH_ZZ
	ret

itc_OR:
	rcall _OR
	rjmp NEXT
_OR:
	rcall ds_POP_ZZ
	mov temp2L, ZZL
	mov temp2H, ZZH
	rcall ds_POP_ZZ
	or ZZL, temp2L
	or ZZH, temp2H
	rcall ds_PUSH_ZZ
	ret

itc_XOR:
	rcall _XOR
	rjmp NEXT
_XOR:
	rcall ds_POP_ZZ
	mov temp2L, ZZL
	mov temp2H, ZZH
	rcall ds_POP_ZZ
	eor ZZL, temp2L
	eor ZZH, temp2H
	rcall ds_PUSH_ZZ
	ret

itc_NEG:
	rcall _NEG
	rjmp NEXT
_NEG:
	rcall _INV
	ldi ZZL, 1
	clr ZZH
	rcall ds_PUSH_ZZ
	rcall _PLUS
	ret

itc_INV:
	rcall _INV
	rjmp NEXT
_INV:
	rcall ds_POP_ZZ
	com ZZL
	com ZZH
	rcall ds_PUSH_ZZ
	ret



itc_HERE:
	rcall _HERE
	rjmp NEXT
_HERE:
	ldi ZL, low(#rammap(_CP))
	ldi ZH, high(#rammap(_CP))
	ld ZZL, Z+
	ld ZZH, Z+
	rcall ds_PUSH_ZZ
	ret

itc_ALLOT:
	rcall _ALLOT
	rjmp NEXT
_ALLOT:
	ldi ZL, low(#rammap(_CP))
	ldi ZH, high(#rammap(_CP))
	ld ZZL, Z+
	ld ZZH, Z+
	rcall ds_PUSH_ZZ
	rcall _PLUS
	rcall ds_POP_ZZ
	ldi ZL, low(#rammap(_CP))
	ldi ZH, high(#rammap(_CP))
	st Z+, ZZL
	st Z+, ZZH
	ret

itc_EXECUTE:
	rcall _EXECUTE
	rjmp NEXT
_EXECUTE:
	rcall ds_POP_ZZ
	movw YL, ZZL
	ld ZL, Y+
	ld ZH, Y+
	ijmp
	ret


itc_COLON:
	rcall _COLON
	rjmp NEXT
_COLON:
	ret

itc_SEMICOLON:
	rcall _SEMICOLON
	rjmp NEXT
_SEMICOLON:
	ret



itc_EQUAL:
	rcall ds_POP_ZZ
	mov temp2L, ZZL
	mov temp2H, ZZH
	rcall ds_POP_ZZ
	cp ZZL, temp2L
	cpc ZZH, temp2H
	brne itc_0
	rjmp itc_FFFF


itc_0:
	clr ZZL
	clr ZZH
	rcall ds_PUSH_ZZ
	rjmp NEXT

itc_FFFF:
	ser ZZL
	ser ZZH
	rcall ds_PUSH_ZZ
	rjmp NEXT

itc_C_AT:
	rcall _C_AT
	rjmp NEXT
_C_AT:
	rcall ds_POP_ZZ
	movw ZL, ZZL
	ld ZZL, Z
	clr ZZH
	rcall ds_PUSH_ZZ
	ret

itc_C_EXCLAM:
	rcall _C_EXCLAM
	rjmp NEXT
_C_EXCLAM:
	movw tempL, XL
	movw XL, dspL
	ld ZH, -X
	ld ZL, -X
	ld ZZH, -X
	ld ZZL, -X
	st Z, ZZL
	movw dspL, XL
	movw XL, tempL
	ret

itc_2STAR:
	rcall _2STAR
	rjmp NEXT
_2STAR:
	rcall ds_POP_ZZ
	lsl ZZL
	rol ZZH
	rcall ds_PUSH_ZZ
	ret

itc_INVERT:
	rcall _INVERT
	rjmp NEXT
_INVERT:
	rcall ds_POP_ZZ
	com ZZL
	com ZZH
	rcall ds_PUSH_ZZ
	ret

itc_2SLASH:
	rcall _2SLASH
	rjmp NEXT
_2SLASH:
	rcall ds_POP_ZZ
	asr ZZh
	ror ZZl
	rcall ds_PUSH_ZZ
	ret



;>python
#addMacro("load","""
#	ldi ZL, low(@1)
#	ldi ZH, high(@1)
#	ld @0L, Z+
#	ld @0H, Z+
#""")



addMacro("subwi","""
	subi @0L, low(@1)
	sbci @0H, high(@1)
""")

addMacro("addwi","""
	subi @0L, low(-@1)
	sbci @0H, high(-@1)
""")

;>endpy



; Змінює >IN - пропускає прогалики і 0. Якщо кінець рядку, то >IN = #TIB
trailing:
	`load source, #rammap(TIB)
	`load count, #rammap(_IN)
	`load temp, #rammap(CTIB)

	add tempL, sourceL ; temp = TIB + CTIB   equ    end of line
	adc tempH, sourceH

	movw ZL, sourceL
	add ZL, countL     ; Z = TIB + _IN    equ   current
	adc ZH, countH

	check_if_line_ended:
		cp ZL, tempL
		; brne is_current_char_space
		cpc ZH, tempH
		 brne is_current_char_space
		; oh no! line ended!
		rjmp finish_trailing
	is_current_char_space:
		ld temp, Z+
	  	cpi temp, 32
		 breq check_if_line_ended
	  	cpi temp, 0
		 breq check_if_line_ended
		; current char is not space!
		sbiw ZL, 1
  finish_trailing:
	sub ZL, sourceL
	sbc ZH, sourceH
	movw countL, ZL

	ldi ZL, low(#rammap(_IN))
	ldi ZH, high(#rammap(_IN))
	st Z+, countL
	st Z+, countH

	ret


; Копіює рядок з одного місця в інше, обертаючи його дзеркально
; ( addr-from addr-to -- len-in-cells )
itc_REVERSESTR:
	rcall ds_POP_ZZ
	movw destL, ZZL
	rcall ds_POP_ZZ
	movw sourceL, ZZL

	rcall reversestr

	movw ZZL, countL
	rcall ds_PUSH_ZZ
	rjmp NEXT
reversestr:
	movw ZL, sourceL
	clr countH
	ld countL, Z
	lsr countL
	inc countL

	lsl countL
	add destL, countL
	adc destH, countH
	lsr countL

	clr temp
	push XL
	push XH
	movw XL, sourceL
	movw ZL, destL
	rev_for_each:
		ld tempL, X+
		ld tempH, X+
		st -Z, tempL
		st -Z, tempH
		inc temp
		cpse temp, countL
	rjmp rev_for_each
	pop XH
	pop XL
	ret


itc_TOKEN:
	`load temp, #rammap(_CP)
	`addwi temp, 1000
	movw destL, tempL
	rcall word

	`load ZZ, #rammap(_CP)
	`addwi ZZ, 1000
	rcall ds_PUSH_ZZ
	rjmp NEXT
word:
	rcall trailing
	push XL
	push XH

	`load source, #rammap(TIB)
	`load count, #rammap(_IN)
	`load temp, #rammap(CTIB)

	add tempL, sourceL
	adc tempH, sourceH

	movw ZL, sourceL
	add ZL, countL
	adc ZH, countH

	movw XL, destL
	adiw XL, 1

	check_if_line_ended_word:
		cp ZL, tempL
		 ;brne is_current_char_space_word
		cpc ZH, tempH
		 brne is_current_char_space_word
		; oh no! line ended!
		rjmp finish_word
	is_current_char_space_word:
		ld temp, Z+
	  	cpi temp, 32
		 breq endd
	  	cpi temp, 0
		 breq endd
		st X+, temp
		rjmp check_if_line_ended_word
	endd:
		; current char is not space!
		sbiw XL, 1
		sbiw ZL, 1
  finish_word:
	sub ZL, sourceL
	sbc ZH, sourceH
	movw countL, ZL
	sub XL, destL
	sbc XH, destH

	movw ZL, destL
	st Z+, XL

	ldi ZL, low(#rammap(_IN))
	ldi ZH, high(#rammap(_IN))
	st Z+, countL
	st Z+, countH
	pop XH
	pop XL
	ret

interpret:
	rcall trailing

	`load count, #rammap(_IN)
	`load temp, #rammap(CTIB)

	cp tempL, countL
	 brne noteol
	cp tempH, countH
	 brne noteol

	;rcall USART_SendACK
	rjmp UART_WAIT ; EOL, wait for input!
    looop:
		ldi temp, 0xFF
		sts LEDs, temp
		rjmp looop ; EOL!!!

   noteol:
	`load dest, #rammap(_CP)
	movw tempL, destL
	`addwi temp, 200
	movw destL, tempL
    rcall word

	push XH
	push XL
		rcall find
	pop XL
	pop XH

	rcall ds_POP_ZZ
	;rjmp intepret_execute

	movw ZL, ZZL
	sbiw ZL, 3
	ld temp, Z
	andi temp, 0x80
	cpi temp, 0
	  brne intepret_execute

	ldi ZL, low(#rammap(STATE))
	ldi ZH, high(#rammap(STATE))
	ld temp, Z
	cpi temp, 0
	  breq intepret_execute

interpret_compile:
	rcall ds_PUSH_ZZ
	ldi ZZL, low(#rammap(mem_COMMA))
	ldi ZZH, high(#rammap(mem_COMMA))

intepret_execute:
	; EXECUTE
	ldi XL, low(#rammap(mmRET))
	ldi XH, high(#rammap(mmRET))
	movw YL, ZZL
	ld ZL, Y+
	ld ZH, Y+
	ijmp

gethere:
	rjmp interpret


;source, dest (LINK)
compare:
	movw XL, sourceL
	movw ZL, destL
	ld temp2, X
	inc temp2
	compare_each:
		ld tempL, X+
		ld tempH, -Z
		andi tempH, 0b01111111
		cp tempL, tempH
		  brne notequal
		dec temp2
		cpi temp2, 0
		 breq equal
	rjmp compare_each


find:
	lds ZL, #rammap(LATEST)
	lds ZH, #rammap(LATEST)+1
	lds sourceL, #rammap(_CP)
	lds sourceH, #rammap(_CP)+1
	movw tempL, sourceL
	`addwi temp, 200
	movw sourceL, tempL
	cycle:
		movw ZZL, ZL
		ld temp2H, -Z
		ld temp2L, -Z
		movw destL, ZL
		rjmp compare
	equal:
		; ZZL - CFA of found word
		rcall ds_PUSH_ZZ
		ret
	notequal:
		cpi temp2L, 0
		 brne continue
		cp temp2L, temp2H
		 brne continue
		rjmp notfound
	continue:
		movw ZL, temp2L
		rjmp cycle
		ret


notfound:
		ldi temp, 0x82
		sts LEDs, temp
		rjmp notfound






itc_NUMBER:
	`load temp, #rammap(BASE)
	clr destL ; result
	clr destH
	ldi temp2L, 1
	clr temp2H
	rcall ds_POP_ZZ
	movw ZL, ZZL
	ld countL, Z+
	clr countH
	add ZL, countL
	adc ZH, countH
	  parse_LOOP:
		ld temp, -Z
		subi temp, 0x30
		cpi temp, 10
		 brge maybe_alphabet
		rjmp operatio
		 maybe_alphabet:
		  subi temp, 7
		operatio:
		cpi temp, 0
		 brlt number_error
		cp temp, tempL
		 brge number_error

		; dest += temp * temp2 (8bit*16bit)
			push temp2L
			push temp2H
			mul temp2L, temp ; Multiply LSB
			mov ZZL, R0 ; copy result to result register
			mov ZZH, R1
			mul temp2H, temp ; Multiply MSB
			add ZZH, R0 ; add LSB result to result byte 2
			add destL, ZZL
			adc destH, ZZH
			pop temp2H
			pop temp2L

		; temp2 = temp2 * tempL (16bit*8bit)
			mul temp2L, tempL ; Multiply LSB
			mov ZZL, R0 ; copy result to result register
			mov ZZH, R1
			mul temp2H, tempL ; Multiply MSB
			add ZZH, R0 ; add LSB result to result byte 2
			movw temp2L, ZZL
		inc countH
		cpse countL, countH
	  rjmp parse_LOOP
	movw ZZL, destL
	rcall ds_PUSH_ZZ
	rjmp NEXT

number_error:
	rjmp notfound
