
all: Test

Test: Lexer.hs Parser.hs *.hs
	ghc -i.:.. Test

Lexer.hs: Lexer.x
	alex Lexer.x

Parser.hs: Parser.y
	happy Parser.y

clean:
	rm Test *.o *.hi Lexer.hs Parser.hs

