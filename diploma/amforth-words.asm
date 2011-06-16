;== INVERT
`HEADER "INVERT", itc_INVERT, mem_INVERT

itc_INVERT:
	rcall _INVERT
	rjmp NEXT
_INVERT:
	rcall ds_POP_ZZ
	com ZZL
	com ZZH
	rcall ds_PUSH_ZZ
	ret


;== AND
`HEADER "AND", itc_AND, mem_AND

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


;== NEGATE
;; FORTH ver
`HEADER "NEGATE", ENTER, mem_NEGATE
`COMPILE_RAW "INVERT 1+ EXIT"


;== 2/
`HEADER "2/", itc_2SLASH, mem_2SLASH

itc_2SLASH:
	rcall _2SLASH
	rjmp NEXT
_2SLASH:
	rcall ds_POP_ZZ
	asr ZZh
	ror ZZl
	rcall ds_PUSH_ZZ
	ret


;== 2*
`HEADER "2*", itc_2STAR, mem_2STAR

itc_2STAR:
	rcall _2STAR
	rjmp NEXT
_2STAR:
	rcall ds_POP_ZZ
	lsl ZZL
	rol ZZH
	rcall ds_PUSH_ZZ
	ret


!!!!!!!!!!!!!!;== EXECUTE
`HEADER "EXECUTE", itc_EXECUTE, mem_EXECUTE

itc_EXECUTE:
	rcall _EXECUTE
	rjmp NEXT
_EXECUTE:
	rcall ds_POP_ZZ
	asr ZZh
	ror ZZl
	rcall ds_PUSH_ZZ
	ret


;== BL
;; FORTH ver
`HEADER "BL", ENTER, mem_BL
`COMPILE_RAW "LIT 32 EXIT"


;== NOOP
;; FORTH ver
`HEADER "NOOP", ENTER, mem_NOOP
`COMPILE_RAW "EXIT"


;== CELL+
;; FORTH ver
`HEADER "CELL+", ENTER, mem_CELLPLUS
`COMPILE_RAW "LIT 2 + EXIT"


;== CELLS
;; FORTH ver
`HEADER "CELLS", ENTER, mem_CELLS
`COMPILE_RAW "2* EXIT"




;== =
`HEADER "=", itc_EQUAL, mem_EQUAL

itc_EQUAL:
	rcall ds_POP_ZZ
	mov temp2L, ZZL
	mov temp2H, ZZH
	rcall ds_POP_ZZ
	cp ZZL, temp2L
	cpc ZZH, temp2H
	brne itc_0
	rjmp itc_FFFF


;== 1
`HEADER "1", itc_FFFF, mem_FFFF

itc_FFFF:
	ser ZZL
	ser ZZH
	rcall ds_PUSH_ZZ
	rjmp NEXT


