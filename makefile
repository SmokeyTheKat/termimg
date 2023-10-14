all:
	dune build --profile release
install: all
	dune install --profile release --prefix=/home/william/.local
depens:
	opam install dune camlimages terminal_size parany
