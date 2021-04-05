
TARGETS1+=gutenberg-11.txt # Alice's Adventures in Wonderland, by Lewis Carroll
TARGETS1+=gutenberg-76.txt # Adventures of Huckleberry Finn by Mark Twain
TARGETS2+=gutenberg-1342.txt # Pride and Prejudice by Jane Austen (338)
TARGETS2+=gutenberg-345.txt # Dracula by Bram Stoker (170)

#all: $(TARGETS)
all: validate example.in testdata.in example.out testdata.out

clean:
	rm -f testdata.*
	rm -f example.*
	rm -f *.out
	rm -f gutenberg-*.txt
	rm -f *-head.txt *-sort.txt


gutenberg-%.txt: get_gutenberg.pl
	perl get_gutenberg.pl $*

%.txt: %.txt.bz2
	bunzip2 < $^ > $@
%-head.txt: %.txt
	# Take the first couple of items
	head -n20 $^ > $@
%-sort.txt: %.txt sort_lines_by_length
	# Sort by decreasing length
	./sort_lines_by_length < $< > $@

sort_lines_by_length: sort_lines_by_length.cpp
	g++ -O2 $^ -o $@

example.in: $(TARGETS1) concatWiki37s-head.txt
	cat $^ > $@
testdata.in: $(TARGETS2) concatWiki37-1s-head.txt nare-strings.txt
	cat $^ > $@



# validate
validate: testdata.in example.in
	ghc --make -O2 ValidateInput.hs -i../../../tools
	./ValidateInput < testdata.in
	./ValidateInput < example.in
	touch $@


# generate outputs

REFERENCE_IMPL = compression-twan-CamelCase

../impl/$(REFERENCE_IMPL): ../impl/$(REFERENCE_IMPL).*
	make -s -C ../impl
../runner/test-compressor: ../runner/*.hs
	make -s -C ../runner

%.out: %.in ../runner/test-compressor ../impl/$(REFERENCE_IMPL)
	../runner/test-compressor ../impl/$(REFERENCE_IMPL) < $< > $@
