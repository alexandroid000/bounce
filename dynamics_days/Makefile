all: \
    	Nilles_poster.pdf

%.pdf: %.tex
	pdflatex $<
	bibtex $*
	pdflatex $<
	pdflatex $<
