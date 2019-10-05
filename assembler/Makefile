
all: assembler

assembler: assembler.o plugin.o utils.o
	$(CC) -o $@ $^

assembler.o: assembler.c assembler.h plugin.h
	$(CC) -c -o $@ $<

plugin.o: example_plugin.c assembler.h plugin.h utils.h
	$(CC) -c -o $@ $<

utils.o: utils.c utils.h
	$(CC) -c -o $@ $<

.PHONY: clean

clean:
	rm *.o

.PHONY: lint tidy check

lint: tidy check

tidy:
	 clang-tidy *.[ch]
	
check:
	 cppcheck *.[ch]

