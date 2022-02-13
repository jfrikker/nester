.PHONY: clean

game: game.o mapper.o
	gcc -o $@ $^

game.o: game.ll
	llc -O3 -filetype=obj -o $@ $^

mapper.o: ../../hardware/apple.c
	clang -O3 -c -o $@ $^

game.ll: apple30th.bin
	nester llvm -O -m apple $^ $@

game.as: apple30th.bin
	nester disassemble -m apple $^ $@

clean:
	rm -f *.ll *.as *.hi *.o *.a game
