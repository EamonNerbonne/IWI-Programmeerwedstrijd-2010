
all: javadecompiler-ando.class javadecompiler-twan javadecompiler-twan-licht

clean:
	rm -f *.class
	rm -f *.exe
	rm -f *.o
	rm -f *.hi

%.class : %.java
	javac $<

javadecompiler-twan: javadecompiler-twan.hs
	ghc --make -O2 -o $@ $^
javadecompiler-twan-licht: javadecompiler-twan-licht.hs
	ghc --make -O2 -o $@ $^
