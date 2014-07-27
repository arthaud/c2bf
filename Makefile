all: build

build:
	ocamlbuild -pkg str c2bf.top

clean:
	ocamlbuild -clean
