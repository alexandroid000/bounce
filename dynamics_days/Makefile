all: \
    	Nilles_poster.pdf

%.pdf: %.tex
	bibtex $*
	pdflatex $<
	pdflatex $<
