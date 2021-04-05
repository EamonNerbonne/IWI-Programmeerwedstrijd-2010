
all: overview.dvi overview-iwi.dvi

.PHONY: overview.tex overview-iwi.tex

overview.tex:
	perl make-overview.pl . 'example' > $@

overview-iwi.tex:
	perl make-overview.pl iwi2010 > $@

%.dvi: %.tex
	latex -quiet $<
%.pdf: %.tex
	pdflatex -quiet $<
