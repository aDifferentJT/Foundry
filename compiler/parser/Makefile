
all: Main

Main: Lexer.hs Parser.hs *.hs
	ghc Main

Lexer.hs: Lexer.x
	alex Lexer.x

Parser.hs: Parser.y
	happy Parser.y

clean:
	rm Main *.o *.hi Lexer.hs Parser.hs

