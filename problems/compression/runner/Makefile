
all: test-compressor

clean:
	rm -f *.class
	rm -f *.exe
	rm -f *.o
	rm -f *.hi

test-compressor: run-compressor.hs
	ghc --make -O2 -o $@ $^
