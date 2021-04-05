
CAT_INPUT = perl ../../../tools/cat-input.pl

all: testdata.in testdata.out example.out validate

clean:
	rm -f testdata.*
	rm -f generated.*
	rm -f *.out
	rm -f MkInput
	rm -f *.class *.hi *.o *.exe


# generate inputs
MkInput: MkInput.cpp
	g++ $^ -o $@
MkInput2: MkInput.hs
	ghc --make -O2 MkInput.hs -o $@ -i../../../tools

generated.in: MkInput
	./MkInput 10 2 10 5 > $@.2
	./MkInput 10 3 10 100 > $@.3
	./MkInput 10 4 19 100 > $@.4
	./MkInput 10 5 19 50 > $@.5
	./MkInput 10 6 19 399 > $@.6
	./MkInput 10 7 10 399 > $@.7
	./MkInput 100 8 10 20 > $@.8
	$(CAT_INPUT) $@.2 $@.3 $@.4 $@.5 $@.6 $@.7 $@.8 > $@
	rm $@.*

generated2.in: MkInput2
	./MkInput2 > $@

# combine inputs
testdata.in: example.in generated.in generated2.in geval.txt
	$(CAT_INPUT) $^ > $@


# validate
validate: testdata.in example.in
	ghc --make -O2 ValidateInput.hs -i../../../tools
	./ValidateInput < testdata.in
	./ValidateInput < example.in
	touch $@


# generate outputs

REFERENCE_IMPL = settlethebill-jasper

../impl/$(REFERENCE_IMPL): ../impl/$(REFERENCE_IMPL).*
	make -s -C ../impl

%.out: %.in ../impl/$(REFERENCE_IMPL)
	../impl/$(REFERENCE_IMPL) < $< > $@
