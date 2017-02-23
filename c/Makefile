CC = clang
CFLAGS = -Wall -O2 -m32

.PHONY: build
build: bfc
	
bfc: bfc.c
	$(CC) $(CFLAGS) -o $@ $<

.PHONY: clean
clean:
	rm -f bfc hello.c hello

.PHONY: test
test: hello
	./hello
	
hello: hello.c
	$(CC) $(CFLAGS) -o $@ $<

hello.c: bfc
	./$< ../bf/hello.bf > $@
