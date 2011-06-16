import sys, os

source_text = "demo0"

demo0 = ""

# Базова демонстрація керування периферією
demo1 = """
	1 ->IND
	1 ->LEDS
"""
# Базова арифметика
demo2 = """
	1 1 + 2* 2* 1 - ->IND
"""

demo3 = """
	: 7
		1 1 + 2* 2* 1 -
	;
	7 1 + ->IND
"""

# Розширення мови
demo4 = """
	: +1    1 + ;
	: =     ->IND ;
	1 +1 =
"""

# Інтерпретація
demo5 = """
	0 ->LEDS 0 ->IND
	1 2* 2* ->portA
	0 C@ 1 + DUP 1 2* 2* 2* 2* 2* AND INVERT 2/ 2/ ->portC 0 C!
	0 >IN !  Тут виходить вічний цикл, тому український текст буде проігноровано
"""
# не забудь зробити RESET

# Вхідний потік і числа
demo6 = """
	COLD

	TOKEN Cool_word!
	TOKEN 120 >NUMBER ->IND
"""

# Змінні, інтепретація/компіляція, зміна семантики
demo7 = """
	COLD

	VARIABLE oldBase
	BASE @ oldBase !

	: DECIMAL    [ TOKEN 10 >NUMBER ] LITERAL BASE ! ;
	: HEX        [ TOKEN 16 >NUMBER ] LITERAL BASE ! ;

	: 0x
		BASE @ oldBase !
		HEX
		TOKEN >NUMBER
		oldBase @ BASE !
	;

	0x 16 ->INDL
	TOKEN 16 >NUMBER ->INDH

"""
"""


	: 0b
		BASE @ oldBase !
		1 1 + BASE !
		TOKEN >NUMBER
		oldBase @ BASE !
	;

	0b 11000011 ->LEDS
"""
demo_complex = """
	COLD 0 ->IND 1 ->LEDS

	: 1+   1 + ;
	: num TOKEN >NUMBER ;
	: CELL+ CELL + ;

	VARIABLE ledState      0 ledState !
	VARIABLE 5x7ledState   0 5x7ledState !
	VARIABLE loopBack      0 loopBack !
	VARIABLE oldBase       BASE @ oldBase !


	: DECIMAL    [ TOKEN 10 >NUMBER ] LITERAL BASE ! ;
	: HEX        [ TOKEN 16 >NUMBER ] LITERAL BASE ! ;
	: 0x
		BASE @ oldBase !
		HEX
		TOKEN >NUMBER
		oldBase @ BASE !
	;

"""
#
"""

	: MAINLOOP
		>IN @ loopBack !
	;


	: LOOPBACK
		loopBack @ >IN !
	;




	num 8 ->portA

	MAINLOOP


		5x7ledState C@ 1 + DUP
		0x 20 AND INVERT 2/ 2/ 2/ 2/ ->portC
		5x7ledState C!

	LOOPBACK

	1 ->IND
"""


source_text = eval(source_text)

def main():
	#inp = sys.argv[1].replace('\\!', '!')
	inp = source_text.replace("\n", " ").replace("\t", " ")
	packets = []
	temp = ""
	for x in inp:
		temp += x
		if len(temp) == 6:
			packets += [temp]
			temp = ""
	if len(temp) > 0:
		temp += " "*(6-len(temp))
		packets += [temp]
	packets += ["     \r"]
	for pack in packets:
		os.system("echo '{0}' > /dev/ttyUSB0".format(pack))

if __name__ == "__main__":
	main()
