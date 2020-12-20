# Makefile for acmart package
#
# This file is in public domain
#
#


default: streams.pdf

CC = xelatex # pdflatex
BC = biber # bibtex 

.PHONY: clean
clean:
	-rm streams.aux streams.log streams.pdf streams.bbl 

squeaky:
	-rm streams.aux 
	-rm streams.log streams.pdf streams.bcf streams.blg streams.toc streams.bbl streams.idx streams.out streams.run.xml  

streams.pdf: streams.tex streams.bib
	$(CC) --shell-escape streams.tex
	$(BC) streams
	$(CC) --shell-escape streams.tex
	$(CC) --shell-escape streams.tex
