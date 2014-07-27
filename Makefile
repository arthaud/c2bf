all: build

build:
	ocamlbuild -pkg str src/c2bf.native

top:
	ocamlbuild -pkg str src/c2bf.top

clean:
	ocamlbuild -clean
