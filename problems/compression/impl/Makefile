
all: compression-twan-dummy compression-twan-CamelCase compression-twan-utable compression-twan-base64 compression-twan-adaptive compression-twan-adaptive2
all: compression-twan-table compression-twan-huffman compression-twan-huffman-ngram
all: compression-jasper-simple

clean:
	rm -f *.class
	rm -f *.exe
	rm -f *.o


%: %.cpp
	g++ -Wall -O2 -o $@ $<

compression-twan-base64: compression-twan-base64.cpp
	g++ -Wall -O2 -o $@ $<

compression-twan-table: compression-twan-table.hs
	ghc --make -O2 -o $@ $<

compression-twan-huffman: compression-twan-huffman.hs
	ghc --make -O2 -o $@ $<

compression-twan-huffman-ngram: compression-twan-huffman-ngram.hs
	ghc --make -O2 -o $@ $<
