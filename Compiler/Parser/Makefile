GHC_FLAGS=-Weverything -Wno-unsafe -Wno-safe -Wno-implicit-prelude

all: Test

Test: Lexer.hs Parser.hs ../*.hs *.hs
	ghc ${GHC_FLAGS} -i.. Test

Lexer.hs: Lexer.x
	alex -g Lexer.x

Parser.hs: Parser.y
	happy -i -g -c -a Parser.y

clean:
	rm Test *.o *.hi Lexer.hs Parser.hs Lexer.info Parser.info

