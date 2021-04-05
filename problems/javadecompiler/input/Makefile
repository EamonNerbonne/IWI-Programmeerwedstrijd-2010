
CAT_INPUT = perl ../../../tools/cat-input.pl

all: testdata.in testdata.out example.out
all: testdata.in-licht testdata.out-licht example.out-licht

clean:
	rm -f testdata.*
	rm -f generated.*
	rm -f *.out *.out-licht
	rm -f MkInput
	rm -f *.class *.hi *.o *.exe


# generate inputs
MkInput: MkInput.hs
	ghc --make -O2 MkInput.hs -i../../../tools

generated.in: MkInput
	./MkInput > $@
generated.in-licht: MkInput
	./MkInput licht > $@


# combine inputs
testdata.in: generated.in example.in
	$(CAT_INPUT) $^ > $@
testdata.in-licht: generated.in-licht example.in-licht
	$(CAT_INPUT) $^ > $@


# validate
.PHONY: validate validate-licht
validate: testdata.in example.in
	ghc --make -O2 ValidateInput.hs -i../../../tools
	./ValidateInput < testdata.in
	./ValidateInput < example.in
validate-licht: testdata.in-licht example.in-licht
	ghc --make -O2 ValidateInputLicht.hs -i../../../tools
	./ValidateInputLicht < testdata.in-licht
	./ValidateInputLicht < example.in-licht



# generate outputs

REFERENCE_IMPL = javadecompiler-twan
REFERENCE_IMPL_LICHT = javadecompiler-twan-licht

../impl/$(REFERENCE_IMPL): ../impl/$(REFERENCE_IMPL).*
	make -s -C ../impl
../impl/$(REFERENCE_IMPL_LICHT): ../impl/$(REFERENCE_IMPL_LICHT).*
	make -s -C ../impl

%.out: %.in ../impl/$(REFERENCE_IMPL)
	../impl/$(REFERENCE_IMPL) < $< > $@
%.out-licht: %.in-licht ../impl/$(REFERENCE_IMPL_LICHT)
	../impl/$(REFERENCE_IMPL_LICHT) < $< > $@
