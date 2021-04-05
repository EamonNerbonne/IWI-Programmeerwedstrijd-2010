BINS = fatslim2-jasper fatslim2-jasper-alt fatslim2-jasper-faulty-1 fatslim2-jasper-faulty-2 fatslim2-Twan
OUTS = fatslim2-jasper.right fatslim2-jasper-alt.right fatslim2-jasper-faulty-1.wrong fatslim2-jasper-faulty-2.wrong fatslim2-Twan.right

all: $(BINS)

check: testdata $(OUTS)

.PHONY: testdata
testdata:
	make -C ../input testdata.in testdata.out

clean:
	rm -f *.class
	rm -f *.exe
	rm -f *.o
	rm -f *.hi

%.class : %.java
	javac $<

%: %.cpp
	g++ -O2 -o $@ $^

%: %.c
	gcc -O2 -o $@ $^

%: %.hs
	ghc --make -O2 -o $@ $^

%.exe: %
	cp $^ $@

%.right: %.exe testdata
	./$< < ../input/testdata.in > $@.try
	diff $@.try ../input/testdata.out > /dev/null
	mv $@.try $@

%.wrong: %.exe testdata
	./$< < ../input/testdata.in > $@.try
	! diff $@.try ../input/testdata.out > /dev/null
	mv $@.try $@
