
CAT_INPUT = perl ../../../tools/cat-input.pl

all: testdata.in testdata.out example.out validate

clean:
	rm -f testdata.*
	rm -f generated.*
	rm -f *.out
	rm -f *.class *.hi *.o *.exe


# generate inputs
MkInput: MkInput.hs
	ghc --make -O2 MkInput.hs -i../../../tools

generated.in: MkInput
	./MkInput > $@


# combine inputs
testdata.in: example.in simple.in generated.in
	$(CAT_INPUT) $^ > $@


# validate
validate: ValidateInput.hs testdata.in example.in
	ghc --make -O2 ValidateInput.hs -i../../../tools
	./ValidateInput < testdata.in
	./ValidateInput < example.in
	touch $@


# generate outputs

REFERENCE_IMPL = fatslim2-jasper

../impl/$(REFERENCE_IMPL): ../impl/$(REFERENCE_IMPL).*
	make -s -C ../impl

%.out: %.in ../impl/$(REFERENCE_IMPL)
	../impl/$(REFERENCE_IMPL) < $< > $@
