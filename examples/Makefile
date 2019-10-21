
.PHONY: clean

clean:
	rm processor_*.v processor_*_assembler lang_server*.log

TESTS := $(wildcard *.fdry)

.PHONY: test test-%

test: $(patsubst %.fdry,test-%,$(TESTS))

test-%: %.fdry
	stack exec Foundry-exe $<

