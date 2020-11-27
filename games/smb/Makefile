.PHONY: clean

game.a: game.o
	ar -csr $@ $^

game.o: game.ll
	llc -O3 -filetype=obj -o $@ $^

game.ll: rom.nes
	nester-exe $^ > $@

game-opt.ll: game.ll
	opt -S -O3 $^ -o $@

clean:
	rm -f *.ll