
all: Test

Test: Lexer.hs Parser.hs ../*.hs
	ghc -i.. Test

Lexer.hs: Lexer.x
	alex -g Lexer.x

Parser.hs: Parser.y
	happy -i -g -c -a Parser.y

clean:
	rm Test *.o *.hi Lexer.hs Parser.hs Lexer.info Parser.info

