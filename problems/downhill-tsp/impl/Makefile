
all: downhill-tsp-twan downhill-tsp-twan-naive downhill-tsp-jasper downhill-tsp-jasper-slow downhill-tsp-jasper-slow2 downhill-tsp-ando.class

clean:
	rm -f *.class
	rm -f *.exe
	rm -f *.o

%.class: downhill-tsp-ando.java
	javac downhill-tsp-ando.java

%: %.cpp
	g++ -Wall -O2 -o $@ $<

%: %.c
	gcc -Wall -O2 -o $@ $<
