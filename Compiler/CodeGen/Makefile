GHC_FLAGS=-fwarn-incomplete-patterns -fwarn-incomplete-uni-patterns

all: Test

Test: *.hs
	ghc ${GHC_FLAGS} -i.. Test

clean:
	rm Test *.o *.hi

