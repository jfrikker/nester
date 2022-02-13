.PHONY: clean

game: game.o mapper.o
	gcc -o $@ $^

game.o: game.ll
	llc -filetype=obj -o $@ $^

mapper.o: ../../hardware/mapper0.c
	clang -O3 -c -o $@ $^

game.ll: game.nes
	nester llvm -O $^ $@

game.as: game.nes
	nester disassemble $^ $@

clean:
	rm -f *.ll *.as *.hi *.o *.a game
