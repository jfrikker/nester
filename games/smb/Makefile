.PHONY: clean

game.a: game.o
	ar -csr $@ $^

game.o: game-opt.ll
	llc -O3 -filetype=obj -o $@ $^

game.ll: game.nes
	nester llvm $^ $@

game-opt.ll: game.ll
	opt -O3 $^ -o $@

game.as: game.nes
	nester decompile $^ $@

clean:
	rm -f *.ll *.as