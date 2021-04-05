
CAT_INPUT = perl ../../../tools/cat-input.pl

all: testdata.in testdata.out example.out validate
all: testdata.in-licht testdata.out-licht validate-licht

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
testdata.in-licht: generated.in-licht example.in
	$(CAT_INPUT) $^ > $@


# validate
validate: testdata.in example.in ValidateInput.hs
	ghc --make -O2 ValidateInput.hs -i../../../tools
	./ValidateInput < testdata.in
	./ValidateInput < example.in
	touch $@
validate-licht: testdata.in-licht example.in ValidateInputLicht.hs
	ghc --make -O2 ValidateInputLicht.hs -i../../../tools
	./ValidateInputLicht < testdata.in-licht
	./ValidateInputLicht < example.in
	touch $@


# generate outputs

REFERENCE_IMPL = downhill-tsp-twan

../impl/$(REFERENCE_IMPL): ../impl/$(REFERENCE_IMPL).*
	make -s -C ../impl

%.out: %.in ../impl/$(REFERENCE_IMPL)
	../impl/$(REFERENCE_IMPL) < $< > $@
%.out-licht: %.in-licht ../impl/$(REFERENCE_IMPL)
	../impl/$(REFERENCE_IMPL) < $< > $@
