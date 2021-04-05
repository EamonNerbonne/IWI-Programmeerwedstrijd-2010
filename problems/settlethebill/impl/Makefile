
all: settlethebill-jasper settlethebill-twan

clean:
	rm -f *.class
	rm -f *.exe
	rm -f *.o
	rm -f *.hi
	rm -f *.manifest

% : %.c
	gcc -O2 -Wall -lm $< -o $@

% : %.cpp
	g++ -O2 -Wall -lm $< -o $@

% : %.hs
	ghc --make -O2 -Wall $< -o $@
