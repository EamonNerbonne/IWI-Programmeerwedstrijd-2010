
all: philosophers-twan philosophers-twan-hs philosophers-sim

clean:
	rm -f *.class
	rm -f *.exe
	rm -f *.o


philosophers-twan: philosophers-twan.c
	gcc -Wall -O2 -o $@ $<

philosophers-sim: philosophers-sim.cpp
	g++ -Wall -O2 -o $@ $<

philosophers-twan-hs: philosophers-twan.hs
	ghc -O2 -o $@ $<
