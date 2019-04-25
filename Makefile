# -*- Makefile -*-

PANDOC			= pandoc
PANDOC_FLAGS	= -s -t latex --toc --listings
LATEX			= platex
LATEX_FLAGS	=
DVIPDFM		= dvipdfmx
DVIPDFM_FLAGS	= -f dvipdfmx-fonts.map

default: report.pdf

report.pdf : report.md report-template.tex
	$(PANDOC) $(PANDOC_FLAGS) --template report-template.tex $< -o $(basename $<).tex
	$(LATEX) $(LATEX_OPTS) $(basename $<).tex
	$(LATEX) $(LATEX_OPTS) $(basename $<).tex
	$(DVIPDFM) $(DVIPDFM_FLAGS) $(basename $<).dvi

clean:
	rm -fr *.dvi *.aux *.out *.log *.toc report.tex
