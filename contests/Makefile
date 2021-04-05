
all: iwi.pdf iwi-zwaar.pdf


clean:
	rm -rf *.dvi
	rm -rf *.pdf

%.dvi: %.tex
	latex -quiet $<
	latex -quiet $<

%.pdf: %.tex
	pdflatex -quiet $<
	pdflatex -quiet $<

.PHONY: iwi.pdf
iwi.pdf: iwi.tex
	pdflatex -quiet $<
	pdflatex -quiet $<

.PHONY: iwi-zwaar.pdf
iwi-zwaar.pdf: iwi-zwaar.tex
	pdflatex -quiet $<
	pdflatex -quiet $<
