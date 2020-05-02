all:
	dune build
clean:
	dune clean
test:
	dune runtest

.PHONY: test all clean
