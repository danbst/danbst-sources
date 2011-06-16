#!/usr/bin/python3

# Макропрепроцесор для проекту StandForth
# Підтримуються наступні команди:
# Загальні
# - pasteMacro - звичайна (або функціональна) макропідстановка
# - gEX - виконання будь-якого Пітонівського коду
# - callLambda - виклик lambda функції по списку аргемнтів
# - addMacro - декларація макросу
# - addFunc - декларація однорядкової макропідставновки
# - pasteLine - звичайна вставка тексту у вихідний файл
# Специфічні
# - addCompiledWord - додати слово до списку компільованих
# - wordDictF - повернути список слів
# - wordDictHasKey - перевірка наявності ключа у словнику
# - printF - вивід на екран відладки текстового повідомлення

import shlex

MACROTEXT = ""

MACROS = {}

FUNCS = {}

lastCFA = 0

def pasteMacro(text1, startNewLine = True, endNewLine = True):
	global MACROTEXT
	if startNewLine:
		MACROTEXT += "\n"
	MACROTEXT += text1
	if endNewLine:
		MACROTEXT += "\n"
	return True

def gEX(execStr):
	exec(execStr, globals())
	return True
def lEX(execStr):
	exec(execStr, locals())
	return True

def callLambda(lam, args):
	if len(args) == 1:
		lam(args[0])
	elif len(args) == 2:
		lam(args[0], args[1])
	elif len(args) == 3:
		lam(args[0], args[1], args[2])
	elif len(args) == 4:
		lam(args[0], args[1], args[2], args[3])

def addMacro(macroName, macroFunc):
	if type("") == type(macroFunc):
		MACROS[macroName] = lambda *args: pasteMacro(pasteWithReplace(macroFunc, args), True, False)
	else:
		MACROS[macroName] = macroFunc

def addFunc(funcName, macroFunc):
	if type("") == type(macroFunc):
		FUNCS[funcName] = lambda *args: pasteWithReplace(macroFunc, args)
	else:
		FUNCS[funcName] = macroFunc

def pasteWithReplace(text, args):
	global MACROTEXT
	for i in range(0, len(args)):
		text = text.replace("@" + str(i), str(args[i]))
	return text

def iif(a,b,c):
	if a:
		return b
	else:
		return c


def preprocessText(text):
	global MACROTEXT

	state = "asm"
	resultPy = ""
	for line in text.splitlines():
		if state == "asm":
			#line = line.strip()
			if line.strip().endswith(";>python"):
				state = "py"
			else:
				while True:
					res = parseForFunc(line)
					if not res["result"] == "found":
						break
					#print(res)
					line = res["preline"] + FUNCS[res["funcname"]](*res["args"]) + res["postline"]

				if line.strip().startswith("`"):
					#print("start     " + line)
					parseMacroLine(line)
					#resultText += MACROTEXT + "\n"
					#MACROTEXT = ""
				else:
					#resultText += line + "\n"
					MACROTEXT += line + "\n"
		elif state == "py":
			if line.endswith(";>endpy"):
				state = "asm"
				#print(resultPy)
				gEX(resultPy)
				resultPy = ""
			else:
				resultPy += line + "\n"
				pass

def pasteLine(line):
	global MACROTEXT
	MACROTEXT += line + "\n"

def parseMacroLine(line):
	for someCode in [ x for x in line.splitlines() if len(x.strip())>0]:
		firstWord = someCode.split()[0]
		restWords = someCode.strip().lstrip(firstWord)
		if firstWord[0] == "`":
			firstWord = firstWord[1:]
			restWordsStrippedSpaces = "".join([x for x in shlex.shlex(restWords)])
			lexer = shlex.shlex(restWordsStrippedSpaces)
			lexer.whitespace = ","
			lexer.commenters = ";"
			lexer.whitespace_split = True
			args = [token for token in lexer]
			if firstWord in MACROS:
				#print(restWords+"\n", firstWord, args)
				callLambda(MACROS[firstWord], args)
			else:
				print("!! Unknown macro")
				print(line)
				exit()


def parseForFunc(line):
	ret = {}
	ret["result"] = "nofunc"
	sharpstart = line.find("#")
	if sharpstart == -1:
		return ret
	else:
		ret["preline"] = line[:sharpstart]
		line = line[sharpstart+1:]
		parentstart = line.find("(")
		if parentstart == -1:
			ret["result"] = "error"
			return ret
		else:
			state = "start"
			oldstate = state
			funcname = line[:parentstart]
			line = line[parentstart+1:]
			parcount = 1
			counter = 0
			endsymb = -1
			for symb in line:
				counter += 1
				if symb == "(" and not state == "string":
					parcount += 1
				elif symb == ")" and not state == "string":
					parcount -= 1
				elif symb == '"' and not state == "string":
					oldstate = state
					curstate = "string"
				elif symb == '"' and state == "string":
					state = oldstate
				else:
					pass
				if parcount == 0:
					endsymb = counter
					break
			if not parcount == 0 or endsymb == -1:
				ret["result"] = "error"
				return ret
			else:
				ret["result"] = "found"
				ret["postline"] = line[endsymb:]
				ret["funcname"] = funcname
				lexer = shlex.shlex("".join([x for x in shlex.shlex(line[:endsymb-1])]))
				lexer.whitespace = ","
				lexer.commenters = ";"
				ret["args"] = [token.strip() for token in lexer]
				return ret

wordDict = {}

def addCompiledWord(w, naddr):
	global wordDict
	wordDict[w] = naddr
	return True

def wordDictF(wwww):
	global wordDict
	return wordDict[wwww]

def wordDictHasKey(k):
	global wordDict
	return k in wordDict

def printF(*x):
	print(*x)
	return True

import os

def main():
	inputFile = open("standforth.asm").read()
	# перегнати код через макропрепроцесор
	preprocessText(inputFile)
	outputFile = open("_gen_standforth.asm", "w")
	outputFile.write(MACROTEXT)
	# скопілювати отриманий файл
	os.system("avra _gen_standforth.asm")
	print("Ok")
	pass

if __name__ == "__main__":
	main()
