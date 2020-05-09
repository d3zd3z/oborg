all:
	dune build
clean:
	dune clean
test:
	dune runtest -f

.PHONY: test all clean
