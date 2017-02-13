test:
	ocamlbuild -pkgs oUnit,yojson,str,ANSITerminal test_game.byte && ./test_game.byte

play:
	ocamlbuild -pkgs oUnit,yojson,str,ANSITerminal main.byte && ./main.byte

check:
	bash checkenv.sh && bash checktypes.sh

clean:
	ocamlbuild -clean
	rm -f checktypes.ml
