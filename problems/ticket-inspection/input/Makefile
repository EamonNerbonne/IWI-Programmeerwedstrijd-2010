
CAT_INPUT = perl ../../../tools/cat-input.pl

#all: testdata.in testdata.out example.out
all:
	@echo Deze opgave wordt toch niet gebruikt

clean:
	rm -f testdata.*
	rm -f generated.*
	rm -f *.out
	rm -f MkInput
	rm -f *.class *.hi *.o *.exe


# generate inputs
#MkInput: MkInput.hs
#	ghc --make -O2 MkInput.hs
#
#generated.in: MkInput
#	./MkInput > generated.in


# combine inputs
testdata.in: generated.in example.in
	$(CAT_INPUT) $^ > $@


# generate outputs

REFERENCE_IMPL = something

../impl/$(REFERENCE_IMPL): ../impl/$(REFERENCE_IMPL).*
	make -s -C ../impl

%.out: %.in ../impl/$(REFERENCE_IMPL)
	../impl/$(REFERENCE_IMPL) < $< > $@
