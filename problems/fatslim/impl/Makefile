
all: fatslim-twan

clean:
	rm -f *.class
	rm -f *.exe
	rm -f *.o
	rm -f *.hi

%.class : %.java
	javac $<

fatslim-twan: fatslim-twan.hs
	ghc --make -O2 -o $@ $^
