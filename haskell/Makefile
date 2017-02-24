CC = stack
CFLAGS = exec ghc --

.PHONY: build
build: bfc
	
bfc: bfc.hs
	$(CC) $(CFLAGS) $<

.PHONY: clean
clean:
	rm -f bfc bfc.o bfc.hi hello hello.hs hello.o hello.hi

.PHONY: test
test: hello
	./hello
	
hello: hello.hs
	$(CC) $(CFLAGS) $<

hello.hs: bfc
	./$< ../bf/hello.bf > $@
