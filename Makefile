all: build

build:
	ocamlbuild -pkg str ctobf.top

clean:
	ocamlbuild -clean
