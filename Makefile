.PHONY: all clean build test

all: build

clean:
	dune clean

test:
	dune runtest

build:
	dune build

