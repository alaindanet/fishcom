################################################################################
#                           Routine for the cluster                            #
################################################################################

MEXT = tex 

## All markdown files in the working directory
SRC = $(wildcard *.$(MEXT))
#@echo $(SRC)

PDFS=$(SRC:.tex=.pdf)

pdf:	$(PDFS)

%.pdf:	%.tex
	pdflatex $< 
	bibtex $<
	pdflatex $<
	mylatex2rtf $< 

all: convert_main convert appendix

convert_main:
	pdflatex manuscript.tex 
	bibtex manuscript
	pdflatex manuscript.tex
	pdflatex manuscript.tex
	mylatex2rtf manuscript.tex
	

convert_appendix:
	pdflatex appendix.tex 
	bibtex appendix
	pdflatex appendix.tex
	mylatex2rtf appendix.tex


