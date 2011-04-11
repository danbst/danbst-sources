;=============================
;--  Макроси для FASM-у, для більшої структурності
;=============================
PUT_CHAR_FUNC = 0x0E
BIOS_VIDEO = 0x10

READ_KEY = 0x00
KEYBOARD = 0x16

;-------------------------------------------------
; MBR clear
;-------------------------------------------------
ORG 7C00h
use16

;++++++++++++++++++++
;--   MAIN    -------
;++++++++++++++++++++

; Ядро операційної системи (інтерпретатора)
; Мінімізував як міг. Пожертвував зрозумілістю
main:			     ; Точка входу у вічний цикл інтерпретатора.  <--------
  call Console_GetString     ; Зчитуємо, що вводить юзер у InputBuffer             |
  mov bx, [last_word]	     ; BX <- адресу останнього слова (див. вниз коду)      |
  search_loop:		     ; Цикл пошуку слова серед інуючих  <----------------  |
	mov si, InputBuffer  ; Наступні 5 команд - перевірка на рівність рядків: | |
	mov di, bx	     ; записаного у полі "Назва" існуючого слова і       | |
	add di, 2	     ; введеного у InputBuffer                           | |
	movzx cx, byte[di]   ;                                                   | |
	repe cmpsb	     ;                                                   | |
	jz EQUAL	     ; Якщо ми знайшли слово, то виконати його!          | |
	mov bx, [bx]	     ; BX <- попереднє слово                             | |
	or bx, bx	     ; Якщо BX != 0 (тобто, слова ще присутні у списку), | |
  jnz search_loop	     ;   то продовжити шукати    ------------------------  |
  mov di, NotFound_code      ; Ой! Уже кінець, а слово не знайдене                 |
			     ; ^[брудний хак] ))                                   |
  EQUAL:		     ;                                                     |
     inc di		     ; обчислюємо початок коду...                          |
     call di		     ; ... і запускаємо його                               |
     call SimplePutEnd	     ; перевід рядка                                       |
jmp main		     ; і все спочатку -------------------------------------

;-----------------
;-- Simple routines for basic I/O
;-----------------
; they change AX

;MODIFIES
; AL - input char will be there
SimpleGetKey:
 mov ah, READ_KEY
 int KEYBOARD
 ret

SimplePutChar:
 mov ah, PUT_CHAR_FUNC
 int BIOS_VIDEO
 ret

;types EOL
SimplePutEnd:
 mov al, 10
 call SimplePutChar
 mov al, 13
 call SimplePutChar
 ret

;------------------
;-- GetString -----
; Gets an #13 ended string from input device (like readln in Pascal)
;------------------
;USES
; DI - destination, where to write string
GetString:
  mov bx, di
  mov byte[bx],-1
  inc di
 mitka:
    call  SimpleGetKey		;считали клавишу в AL
    stosb
    call  SimplePutChar 	;также нужно вывести ее на екран, чтобы юзерь видел, что вводит
    inc   byte[bx]		;длина строки
    cmp   al, 13		;пока не нажмут Enter
 jnz mitka
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
 mov al, '>'
 call SimplePutChar
 mov al, ' '
 call SimplePutChar
 mov di, InputBuffer   ; Ось ядро цієї функції
 call GetString        ; Виділив його коментаріями серед купи декораторів
 call SimplePutEnd
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
PutPASCAL_opt:
 movzx cx, byte[si]
 inc si
 mov ah, PUT_CHAR_FUNC
 next_pascal_char:
     lodsb
     int BIOS_VIDEO
 loop next_pascal_char
 ret

;---------------------------
;--   Different data -------
;---------------------------
; Дані для ядра.

last_word     dw MAND	     ; last_word вказує на останнє визначене слово

;---
; NOTFOUND - повинно бути першим. NotFound_code не видаляти. Інше можна змінити
NOTFOUND:
	dw 0
	db 8,'NOTFOUND'
    NotFound_code:
	   nop
	   mov si, nf_help
	   call PutPASCAL_opt
	   ret
	   nf_help db 29,'Sorry Dave, I cannot do that.'
; Далі йдуть юзерські слова....
HELLO:
	dw NOTFOUND
	db 12,'Hello world!'
	   mov si, hell_help
	   call PutPASCAL_opt
	   ret
	   hell_help db 8,'Hi dude!'
; Mandelbrot plotter, 61 bytes - Tenie Remmel
; Надіюсь, вам сподобається )
MAND:
	dw HELLO
	db 4,'MAND'
	push es
	mov ax, 0
	mov di, 0fffeh
	push	0A000h
	pop	es
	mov	al, 013h	; установка видеорежима
	int	10h		; 320х200 256 цветов
	stosw			; [es:di] <-- ax, di <-- di+2
	mov	cl, 200 	; cl <-- 200
  outer_loop:
	mov	si, 320 	; si <-- 320
    inner_loop:
	mov	bp, 79		; bp <--- макс. число шагов
	xor	bx, bx		; p <-- 0
	xor	dx, dx		; q <-- 0
    complex_loop:
	push	dx		; сохраняем q в стеке
	mov	ax, bx		; ax <-- p
	sub	ax, dx		; ax <-- p - q
	add	dx, bx		; dx <-- q + p
	imul	dx		; dx:ax <-- p^2 - q^2
	mov	al, ah
	mov	ah, dl		; ax <-- (p^2 - q^2) / 256
	pop	dx		; dx <-- q из стека
	xchg	bx, ax		; bx <-- (p^2 - q^2)/256, ax <-- p
	sub	bx, si		; bx <-- (p^2 - q^2)/256 + 256x
	imul	dx		; dx:ax <-- p*q
	shld	dx, ax, 9	; dx <-- p*q / 128
	sub	dx, cx		; dx <-- p*q / 128 + 256*y
	test	dh, dh
	jg	plot_color	; если q >= 256,  ----v
	dec	bp		; bp--
	jne	complex_loop	; если bp != 0, ----^
    plot_color:
	xchg	bp, ax
	stosb			; экран <-- bp
	dec	si		; si--
	jne	inner_loop	; если si != 0, ----^
  loop	  outer_loop		; cx раз   ----^
  pop es

  call SimpleGetKey
  mov ax, 2
  int 10h
  ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Всі юзерські функції повинні описуватись вище даного місця.
; Звідси починаються системні змінні, як наприклад ось ця
; Це для того, щоб не створювати межі для вводу символів.
InputBuffer db 0


times (512-2-($-7C00h)) db 0
db 055H, 0AAH

;=================================================
; Rest 63 sectors
;=================================================
times 512*63 db 0