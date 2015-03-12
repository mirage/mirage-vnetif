.PHONY: all clean

all: 
	ocamlfind ocamlopt vnetif.mli vnetif.ml basic_backend.mli basic_backend.ml -o test -linkpkg -package cstruct,lwt,mirage-types,tcpip

clean:
	rm -f *.o *.cmi *.cmx test
	
