.PHONY: clean

game: main.hs game.a
	stack ghc -- -threaded -rtsopts -with-rtsopts=-N -o $@ $^

game.a: game.o
	ar -csr $@ $^

game.o: game-opt.ll
	llc -O3 -filetype=obj -o $@ $^

game.ll: game.nes
	nester llvm $^ $@

game-opt.ll: game.ll
	opt -O3 $^ -o $@

game.as: game.nes
	nester disassemble $^ $@

clean:
	rm -f *.ll *.as *.hi *.o *.a game
