all:
	dune build --profile release
install: all
	dune install --profile release --prefix=/home/william/.local
tc: all
	./_build/default/termimg.exe /home/william/Pictures/pp.jpg -s 20 20 -c -l
tc2: all
	./_build/default/termimg.exe ./test7.gif -l
