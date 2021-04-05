
all: forklift-ando-cleanesterest.class forklift-twan

clean:
	rm -f *.class
	rm -f *.exe
	rm -f *.o

%.class: forklift-ando-cleanesterest.java
	javac forklift-ando-cleanesterest.java

forklift-twan: forklift-twan.hs
	ghc --make -O2 $^ -o $@
