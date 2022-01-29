.PHONY: clean

game: game.o mapper.o
	gcc -o $@ $^

game.o: game.ll
	llc -O3 -filetype=obj -o $@ $^

mapper.o: ../../hardware/mapper0.c
	clang -O3 -c -o $@ $^

game.ll: game.nes
	nester llvm $^ $@

game-opt.ll: game.ll
	opt -O3 $^ -o $@

game.as: game.nes
	nester disassemble $^ $@

clean:
	rm -f *.ll *.as *.hi *.o *.a game
