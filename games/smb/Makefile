.PHONY: clean

game: game.o
	gcc -o $@ $^

game.o: game-opt.ll
	llc -O3 -filetype=obj -o $@ $^

game.ll: game-part.ll mapper.ll
	llvm-link -o $@ $^

mapper.ll: ../../hardware/mapper0.c
	clang -O3 -emit-llvm -S -o $@ $^

game-part.ll: game.nes
	nester llvm $^ $@

game-opt.ll: game.ll
	opt -O3 $^ -o $@

game.as: game.nes
	nester disassemble $^ $@

clean:
	rm -f *.ll *.as *.hi *.o *.a game
