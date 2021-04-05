
all: tiltmaze-jasper TiltmazeTwan.class tiltmaze-jasper-slow

clean:
	rm -f *.class
	rm -f *.exe
	rm -f *.o

% : %.c
	gcc -O2 -Wall -lm $< -o $@

% : %.cpp
	g++ -O2 -Wall -lm $< -o $@

TiltmazeTwan.class: tiltmaze-twan.java
	javac $^
