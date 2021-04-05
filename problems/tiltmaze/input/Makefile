
CAT_INPUT = perl ../../../tools/cat-input.pl

all: testdata.in testdata.out example.out validate

clean:
	rm -f testdata.*
	rm -f generated.*
	rm -f *.out
	rm -f MkInput
	rm -f *.class *.hi *.o *.exe


# generate inputs
MkInput: MkInput.hs
	ghc --make -O2 MkInput.hs -i../../../tools

exhaustive.in: MkInput
	./MkInput > $@


# combine inputs
testdata.in: simple.in open.in exhaustive.in *x*_*.in example.in
	$(CAT_INPUT) $^ > $@


# validate
.PHONY: validate
validate: testdata.in example.in
	ghc --make -O2 ValidateInput.hs -i../../../tools
	./ValidateInput < testdata.in
	./ValidateInput < example.in


# generate outputs

REFERENCE_IMPL = tiltmaze-jasper

../impl/$(REFERENCE_IMPL): ../impl/$(REFERENCE_IMPL).*
	make -s -C ../impl

%.out: %.in ../impl/$(REFERENCE_IMPL)
	../impl/$(REFERENCE_IMPL) < $< > $@
