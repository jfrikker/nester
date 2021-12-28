.PHONY: clean

game: game.o
	gcc -o $@ $^

game.o: game-opt.ll
	llc -O3 -filetype=obj -o $@ $^

game.ll: game-part.ll mapper.ll
	llvm-link -o $@ $^

mapper.ll: ../../hardware/apple.c
	clang -O3 -emit-llvm -S -o $@ $^

game-part.ll: apple30th.bin
	nester llvm --format apple $^ $@

game-opt.ll: game.ll
	opt -O3 $^ -o $@

game.as: apple30th.bin
	nester disassemble --format apple $^ $@

clean:
	rm -f *.ll *.as *.hi *.o *.a game
