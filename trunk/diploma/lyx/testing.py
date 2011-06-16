import sys, os

source_text = """
	0 ->LEDS 0 ->IND
	1 2* 2* ->portA
	0 C@ 1 + DUP 1 2* 2* 2* 2* 2* AND INVERT 2/ 2/ ->portC 0 C!
	0 >IN ! LOOP FOREVA!!!!
"""


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
		os.system("echo '{0}' > /dev/ttyUSB1".format(pack))

if __name__ == "__main__":
	main()
