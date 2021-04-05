
all: minefield-sander minefield-sander-float minefield-twan minefield-jasper

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

minefield-twan: minefield-twan.hs
	ghc --make -O2 $< -o $@

