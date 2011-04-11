;-------------------------------------------------
; MBR Loader level 1
;-------------------------------------------------
ORG 7C00h
mov ax, 0x0200 + 63
mov bx, main
mov cx, 2
mov dx, 0
int 13h
jmp main
times (512-2-($-7C00h)) db 0
db 055H, 0AAH

;++++++++++++++++++++
;--   MAIN    -------
;++++++++++++++++++++
main:
	call Console_GetString
	    .global_loop:
		mov al, 32
		call skipTrailing4th
		mov bx, [_In]
		cmp bx, [Span]
		  jz .end

		mov si, TempToken
		call getWord4th
		mov bx, [Latest]
		    .search_loop:
			mov si, bx
			mov di, TempToken
			movzx cx, byte[si]
			and cx, 0x7F
			cmp cx, 0
			  jz .skip
			cmp cl, byte[di]
			  jnz .skip
			inc si
			inc di
			repe cmpsb
			  jz .equal

		      .skip:
			movzx ax, byte[bx]
			and ax, 0x7F
			add bx, ax
			add bx, 1
			mov bx, [bx]

			cmp bx, 0
		    jnz .search_loop

		mov si, TempToken
		call ParseNumber
		  jc .notfound
		cmp [State], 0
		  jz .needPush
	      .needCompileLit:
		push bx
		mov bx, [_Dp]
		mov byte[bx], 0xB8
		inc bx
		mov word[bx], ax
		add bx, 2
		mov byte[bx], 0xE8
		inc bx
		mov ax, PushAX
		sub ax, bx
		sub ax, 2
		mov word[bx], ax
		add bx, 2
		mov [_Dp], bx
		pop bx
		  jmp .endcall
	      .needPush:
		call PushAX
		  jmp .endcall

	      .notfound:
		mov [State], 0
		mov si, TempToken
		call PutPascal
		mov si, .notFoundStr
		call PutPascal
		  jmp main

	      .equal:
		movzx ax, byte[bx]
		mov dx, ax
		and ax, 0x7F
		add bx, ax
		add bx, 3
		test dx, 0x80
		  jnz .needCall
		cmp [State], 0
		  jz .needCall
	      .needCompile:
		mov ax, bx
		mov bx, [_Dp]
		sub ax, bx
		sub ax, 3
		push bx
		mov byte[bx], 0xE8
		inc bx
		mov [bx], ax
		add bx, 2
		mov [_Dp], bx
;                mov byte[bx], 0xC3
		pop bx
		;call bx
		jmp .endcall
	      .needCall:
		mov [SavedSP], sp
		pushf
		pusha
		call bx
		popa
		popf
	      .endcall:

		mov bx, [_In]
		cmp bx, [Span]
	    jnz .global_loop
      .end:
	mov si, .okStr
	call PutPascal
jmp main
.okStr db 5, ' Ok',13,10
.notFoundStr db 4,' ?',13,10

Base	dw 10
Latest	dw LAST
_S0	dw 0x9999
_Sp	dw 0x9999
State	dw 0
Tib	dw 0x7000
_In	dw 0
Span	dw 0
_Dp	dw LAST + 4
SavedSP dw 0
SavedLatest dw 0

;types EOL
SimplePutEnd:
	push ax
	mov ah, 0x0E
	mov al, 10
	int 0x10
	mov al, 13
	int 0x10
	pop ax
	ret

;--------------------
;--   Console_PutPASCAL  -------
; Puts a string with length in zero-index byte to screen and types EOL
;--------------------
; USES
; SI - adress of string
; MODIFIES
; SI - end of string
; CX - 0
; AX - trash
PutPascal:
	movzx cx, byte[si]	      ; довжина рядка --> CL, 0 --> CH
	cmp cx, 0
	  jz .end
	inc si			      ; переміщуємось на перший символ
	mov ah, 0x0E
	    .next_pascal_char:
		lodsb		      ; наступний символ --> AL; SI++ (або SI--)
		int 0x10
	    loop .next_pascal_char
      .end:
	ret
Type:
	cmp cx, 0
	  jz .end
	mov ah, 0x0E
	    .next_pascal_char:
		lodsb		      ; наступний символ --> AL; SI++ (або SI--)
		int 0x10
	    loop .next_pascal_char
      .end:
	ret


;--------------------------
;-- Console_GetString -----
; Gets an #13 ended string, but in console-like view. Sets SI to adress of input buffer
;--------------------------
;USES
; nothing
;MODIFES
; AX - trash
; DI - adress of last free memory
; SI - adress of current word, input buffer
Console_GetString:
	pusha
	mov si, .input
	call PutPascal
	call Get4ThString
	call SimplePutEnd
	popa
	ret
	.input db 2, '> '

;-------------------------------------------------------------------------------
;--   GetString
; Зчитує з клавіатури рядок у вказане місце в пам'яті
;-------------------------------------------------------------------------------
;ВХІД
;ВИХІД
Get4ThString:
	push di
	push ax
	mov di, [Tib]
	    mitka:
		mov ah, 0x10
		int 0x16
		cmp al, 32	     ; якщо клавіша - не друкований символ,
		  jb .notPrintable   ;  то перевіряємо її на контролюючі клавіші
		stosb
		mov ah, 0x0E
		int 0x10
	    jmp mitka
	      .notPrintable:
	      .backspace:
		cmp al, 8	     ; якщо це не BackSpace,
		  jnz .enter	     ;    то перевіряємо далі
		cmp di, [Tib]	   ; а якщо довжина рядка нульова,
		  jle mitka	      ;    то не обробляємо клавішу
		dec di		     ; зменшуємо довжину рядка на 1
		mov ah, 0x0E
		int 0x10   ; виводимо послідовність символів
		mov al, 32	     ; BackSpace, Space, BackSpace
		int 0x10   ; BackSpace - це всього лиш перевід курсору
		mov al, 8	     ;   назад
		int 0x10   ;
		jmp mitka
	      .enter:
		cmp   al, 13		    ;пока не нажмут Enter
	    jnz mitka
	mov al, 32
	stosb
	sub di, [Tib]
	mov [_In], 0		   ;  рядка)
	mov [Span], di		     ;  рядка)
	pop ax
	pop di
	ret

; al - symbol to skip
skipTrailing4th:
	pusha
	mov di, [Tib]
	add di, [_In]
	mov cx, [Span]
	sub cx, [_In]
	cmp cx, 0	    ; якщо рядок уже закінчився то виходимо
	  jz .end
	repe scasb	    ; проітерувати по прогаликам
	  jz .end	    ; REPE працює, доки у нас в рядку прогалики, тобто, доки
			    ; ZF=1. Якщо ми вийшли з REPE і у нас досі ZF=1, то це значить тільки
			    ; одне - кінчився рядок, CX=0. Ну, якщо в кінці рядка одні прогалики,
			    ; то їх можна ігнорувати і виходити з процедури
	inc cx
      .end:
	mov ax, [Span]
	sub ax, cx
	mov [_In], ax
	popa
	ret

;al - end symbol
;si - adress where to put token
getWord4th:
	pusha
	mov di, [Tib]
	add di, [_In]
	mov cx, [Span]
	sub cx, [_In]
	cmp cx, 0	    ; якщо рядок уже закінчився то виходимо
	  jz .end
     .looop:
	mov ah, byte[di]
	cmp ah, al
	  jz .end
	inc di
	inc si
	mov byte[si], ah
     loop .looop
      .end:
	sub di, [Tib]
	mov ax, di
	sub ax, [_In]
	mov [_In], di
	sub si, ax
	mov byte[si], al
	add [_In], 1
	popa
	ret

GetToken:
	mov si, TempToken
	mov al, 32
	call skipTrailing4th
	mov bx, [_In]
	mov si, TempToken
	call getWord4th
	ret
;-------------------------------------------------------------------------------
;--   PrintEAX, PrintAX, PrintAL
; Вивести беззнакове число у EAX/AX/AL на екран в поточну позицію курсору.
; Основа числа задається у байті BASE
;-------------------------------------------------------------------------------
;ВХІД
; EAX/AX/AL - число
;ВИХІД
; нічого
PrintEAX:
	pusha
	movzx ebx, word[Base]	   ; Основа числення
	xor ecx, ecx
	xor edx, edx		   ; обнуляємо, оскільки ділене виду EDX:EAX
	    .div_loop:
		div ebx
		cmp dl, 9	   ; якщо частка більша дев'яти,
		 jle .cyfer_system ;    то це цифра,
		add dl, 7	   ;    інакше - перетворюємо у букву
	      .cyfer_system:	   ;
		add dx, 0x0E30	   ; 0..9 і 17..35  ->  '0'..'9' і 'A'..'Z'
				   ; 0E - функція виводу символу (для оптим.)
		push dx 	   ; зберігаємо ASCII цифру у стек
		inc cl		   ; кількість циферок++
		xor edx, edx	   ; лишнє тепер
		or eax, eax	   ; якщо EAX != 0,
	    jnz .div_loop	   ;   то продовжуємо ділити
	    .print_loop:	   ;   інакше - виводимо байти з стеку,
		pop ax		   ;   в CL їхня кількість
		int 0x10
	    loop .print_loop
	popa
	ret
PrintAX:
	push eax
	and eax, 0xFFFF    ; Обнулити старші два байти EAX
	call PrintEAX
	pop eax
	ret
PrintAL:
	push eax
	and eax, 0xFF	   ; Обнулити всі байти EAX окрім наймолодшого
	call PrintEAX
	pop eax
	ret

; di - mem
PrintAXmem:
	pusha
	mov byte[di], 0
	mov bx, word[Base]	; Основа числення
	xor cx, cx
	xor dx, dx		 ; обнуляємо, оскільки ділене виду EDX:EAX
	    .div_loop:
		div bx
		cmp dl, 9	   ; якщо частка більша дев'яти,
		 jle .cyfer_system ;    то це цифра,
		add dl, 7	   ;    інакше - перетворюємо у букву
	      .cyfer_system:	   ;
		add dx, 0x30	 ; 0..9 і 17..35  ->  '0'..'9' і 'A'..'Z'
				   ; 0E - функція виводу символу (для оптим.)
		push dx 	   ; зберігаємо ASCII цифру у стек
		inc cl		   ; кількість циферок++
		inc byte[di]
		xor dx, dx	 ; лишнє тепер
		or ax, ax	 ; якщо EAX != 0,
	    jnz .div_loop	   ;   то продовжуємо ділити
	    inc di
	    .print_loop:	   ;   інакше - виводимо байти з стеку,
		pop ax		   ;   в CL їхня кількість
		stosb
	    loop .print_loop
	popa
	ret


; SI - word to parse
; AX - result
ParseNumber:
	push si
	push dx
	push cx
	movzx cx, byte[si]
	inc si
	mov ax, 0
	mov dx, 0
	mov bx, [Base]
	    .lloop:
		lodsb
		sub al, '0'
		cmp al, 9
		  ja .addit
		jmp .mul
	      .addit:
		sub al, 7
		cmp al, bl
		  ja .error
	      .mul:
		imul dx, bx
		add dx, ax
		  jc .error
	    loop .lloop
	    jmp .end
      .error:
	pop cx
	pop dx
	pop si
	stc
	ret
      .end:
	pop cx
	mov ax, dx
	pop dx
	pop si
	clc
	ret

PushAX:
	pusha
	mov bx, [_Sp]
	mov [bx], ax
	add [_Sp], 2
	popa
	ret

PopAX:
	sub [_Sp], 2
	mov bx, [_S0]
	cmp [_Sp], bx
	  jl .stackerror
	mov bx, [_Sp]
	mov ax, [bx]
	ret
     .stackerror:
	mov bx, [_S0]
	mov [_Sp], bx
	mov sp, [SavedSP]
	mov [State], 0
	call SimplePutEnd
	mov si, .error_message
	call PutPascal
	pop ax
	push main
	ret
	.error_message db 16,"Stack is empty",13,10

TempToken rb 128
HI:
	db 0x82, 'hi'
	dw 0
	   mov si, .test
	   call PutPascal
	   ret
	   .test db 4, 'test'
HELLO:
	db 5, 'hello'
	dw HI
	   mov si, .test
	   call PutPascal
	   ret
	   .test db 4, 'tes2'
TOKEN:
	db 5,'token'
	dw HELLO
	   mov al, ' '
	   call GetToken
	   pusha
	   mov si, TempToken
	   call PutPascal
	   popa
	   ret
SPACE:
	db 5,'space'
	dw TOKEN
	   mov al, ' '
	   mov ah, 0x0E
	   int 0x10
	   ret
SEMICOLON:
	db 4,'semi'
	dw SPACE
	   mov ax, ': '
	   call PushAX
	   ret
PRINTNUM:
	db 1,'.'
	dw SEMICOLON
	   call PopAX
	   call PrintAX
	   ret
SPACES:
	db 6, 'spaces'
	dw PRINTNUM
	   call PopAX
	   pusha
	   cmp ax, 0
	     jz .end
	   mov cx, ax
	   mov al, ' '
	       .print_loop:
		   mov ah, 0x0E
		   int 0x10
	       loop .print_loop
	 .end:
	   popa
	   ret
EMIT:
	db 4, 'emit'
	dw SPACES
	   call PopAX
	   mov ah, 0x0E
	   int 0x10
	   ret
CR:
	db 2, 'cr'
	dw EMIT
	   call SimplePutEnd
	   ret
STRING_PRINT:
	db 2, '."'
	dw CR
	   mov al, '"'
	   mov si, TempToken
	   call getWord4th
	   mov si, TempToken
	   call PutPascal
	   ret
PLUS:
	db 1, '+'
	dw STRING_PRINT
	   push dx
	   call PopAX
	   mov dx, ax
	   call PopAX
	   add ax, dx
	   call PushAX
	   pop dx
	   ret
MINUS:
	db 1, '-'
	dw PLUS
	   push dx
	   call PopAX
	   mov dx, ax
	   call PopAX
	   sub ax, dx
	   call PushAX
	   pop dx
	   ret
DUPP:
	db 3, 'dup'
	dw MINUS
	   call PopAX
	   call PushAX
	   call PushAX
	   ret
DROP:
	db 4, 'drop'
	dw DUPP
	   call PopAX
	   ret
SWAP:
	db 4, 'swap'
	dw DROP
	   push dx
	   call PopAX
	   mov dx, ax
	   call PopAX
	   xchg ax, dx
	   call PushAX
	   mov ax, dx
	   call PushAX
	   pop dx
	   ret
OVER:
	db 4, 'over'
	dw SWAP
	   push dx
	   call PopAX
	   mov dx, ax
	   call PopAX
	   call PushAX
	   xchg ax, dx
	   call PushAX
	   mov ax, dx
	   call PushAX
	   pop dx
	   ret
COMMENT:
	db 0x80+1, '('
	dw OVER
	   mov al, ')'
	   mov si, TempToken
	   call getWord4th
	   ret
FETCH:
	db 1, '@'
	dw COMMENT
	   call PopAX
	   push bx
	   mov bx, ax
	   mov ax, [bx]
	   pop bx
	   call PushAX
	   ret
STOREE:
	db 1, '!'
	dw FETCH
	   call PopAX
	   push dx
	   mov dx, ax
	   call PopAX
	   push bx
	   mov bx, dx
	   mov [bx], ax
	   pop bx
	   pop dx
	   ret
FETCH_BYTE:
	db 2, 'c@'
	dw STOREE
	   call PopAX
	   push bx
	   mov bx, ax
	   movzx ax, byte[bx]
	   pop bx
	   call PushAX
	   ret
STOREE_BYTE:
	db 2, 'c!'
	dw FETCH_BYTE
	   call PopAX
	   push dx
	   mov dx, ax
	   call PopAX
	   push bx
	   mov bx, dx
	   mov byte[bx], al
	   pop bx
	   pop dx
	   ret
TO_COMPILE:
	db 1, ']'
	dw STOREE_BYTE
	   mov [State], -1
	   ret
TO_INTERPRET:
	db 0x80+1, '['
	dw TO_COMPILE
	   mov [State], 0
	   ret
GET:
	db 3, 'get'
	dw TO_INTERPRET
	   mov ah, 0x10
	   int 0x16
	   call PrintAL
	   call SimplePutEnd
	   xchg ah, al
	   call PrintAL
	   call SimplePutEnd
	   ret
C_BASE:
	db 4, 'BASE'
	dw GET
	   mov ax, Base
	   call PushAX
	   ret
C_LATEST:
	db 6, 'LATEST'
	dw C_BASE
	   mov ax, [Latest]
	   call PushAX
	   ret
C_S0:
	db 2, 'S0'
	dw C_LATEST
	   mov ax, [_S0]
	   call PushAX
	   ret
C_SP:
	db 2, 'SP'
	dw C_S0
	   mov ax, [_Sp]
	   call PushAX
	   ret
C_STATE:
	db 5, 'STATE'
	dw C_SP
	   mov ax, State
	   call PushAX
	   ret
C_TIB:
	db 3, 'TIB'
	dw C_STATE
	   mov ax, [Tib]
	   call PushAX
	   ret
C_IN:
	db 2, 'IN'
	dw C_TIB
	   mov ax, _In
	   call PushAX
	   ret
C_SPAN:
	db 4, 'SPAN'
	dw C_IN
	   mov ax, Span
	   call PushAX
	   ret
C_DP:
	db 2, 'DP'
	dw C_SPAN
	   mov ax, _Dp
	   call PushAX
	   ret
_LOAD:
	db 0x80+1, 'L'
	dw C_DP
	   mov [Tib], LOAD_TIB
	   mov [_In], 0
	   mov cx, -1
	   mov di, LOAD_TIB
	   mov al, 0
	   repne scasb
	   neg cx
	   sub cx, 2
	   mov [Span], cx
	   ret
HERE:
	db 4, 'here'
	dw _LOAD
	   mov ax, [_Dp]
	   call PushAX
	   ret
TO_R:
	db 2, '>r'
	dw HERE
	   call PopAX
	   push ax
	   ret
R_OUT:
	db 2, 'r>'
	dw TO_R
	   pop ax
	   call PushAX
	   ret
R_FETCH:
	db 2, 'r@'
	dw R_OUT
	   pop ax
	   push ax
	   call PushAX
	   ret
SEMI_CREATE:
	db 1, ':'
	dw R_FETCH
	   mov ax, [_Dp]
	   mov [SavedLatest], ax
	   pusha
	   mov si, [_Dp]
	   mov al, 32
	   call skipTrailing4th
	   mov bx, [_In]
	   mov si, [_Dp]
	   call getWord4th
	   mov si, [_Dp]
	   movzx ax, byte[si]
	   add si, ax
	   inc si
	   mov ax, [Latest]
	   mov word[si], ax
	   add si, 2
	   mov [_Dp], si
	   mov [State], -1
	   popa
	   ret
SEMI_END:
	db 0x80+1, ';'
	dw SEMI_CREATE
	   mov si, [_Dp]
	   mov byte[si], 0xC3
	   inc si
	   mov [_Dp], si
	   mov [State], 0
	   mov ax, [SavedLatest]
	   mov [Latest], ax
	   ret
PRINTMEM:
	db 2, '.m'
	dw SEMI_END
	   call PopAX
	   mov di, ax
	   call PopAX
	   call PrintAXmem
	   ret
SYMBOLS:
	db 5, 'nemit'
	dw PRINTMEM
	   pusha
	   call PopAX
	   push ax
	   call PopAX
	   cmp ax, 0
	     jz .end2
	   mov cx, ax
	   pop ax
	       .print_loop:
		   mov ah, 0x0E
		   int 0x10
	       loop .print_loop
	   jmp .end
	 .end2:
	   pop ax
	 .end:
	   popa
	   ret
ASCII:
	db 5, 'mword'
	dw SYMBOLS
	   mov si, [_Dp]
	   mov al, 32
	   call getWord4th
	   ret
IMMIDIET:
	db 3, 'imm'
	dw ASCII
	   ret
MEMPRINT:  ; \ ( n addr -- )
	db 3, 'mem'
	dw IMMIDIET
	   call PopAX
	   mov si, ax
	   call PopAX
	   mov cx, ax
	   mov ah, 0x0E
	      @@:
		 lodsb
		 cmp al, 32
		   ja .put
		 mov al, '.'
	       .put:
		 int 10h
	      loop @b
	   ret
LAST:
	db 0
	dw MEMPRINT
rw 1000

LOAD_TIB:
db ' : ascii ( -- n ) mword here 1 + c@ ;                                            '
db ' : .h ( f n -- ) ( print hex ) over here .m here c@ - 48 nemit . ;               '
db ' : put-1 ( a -- next_addr ) dup c@ 2 .h 1 + ;                                    '
db ' : put-2 ( a -- next_addr ) put-1 space put-1 ;                                  '
db ' : put-4 ( a -- next_addr ) put-2 space put-2 ;                                  '
db ' : put-8 ( a -- next_addr ) put-4 space put-4 ;                                  '
db ' : put-line ( a -- next_addr ) dup dup 8 .h space 124 emit put-8 2 spaces put-8  '
db '   124 emit space 16 - 16 swap mem 16 + cr ;                                     '
db ' : dump ( a -- ) put-line put-line put-line put-line put-line put-line put-line put-line '
db '   put-line put-line put-line put-line put-line put-line put-line put-line drop ; '
db '                                             '
db '                                             '
db '                                             '
db '                                             '
db '                                             '
db '                                             '
db '                                             '
db '                                             '
db '                                             '
db 32,0

;=================================================
; Rest 63 sectors
;=================================================
times (512*64-($-7C00h)) db 0