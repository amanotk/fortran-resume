# -*- Makefile -*-

PANDOC         = pandoc -t latex
LATEX          = platex
DVIPDFMX       = dvipdfmx
INTRO_TEMPLATE = ../template/intro.tex
KADAI_TEMPLATE = ../template/kadai.tex

default: intro.pdf kadai

clean:
	rm -rf *.aux *.dvi *.log *.out *.tex

# for building PDF for introduction
intro.pdf: intro.md
	$(PANDOC) -f markdown+hard_line_breaks --template $(INTRO_TEMPLATE) $< -o $(basename $<).tex
	$(LATEX) $(basename $<).tex
	$(LATEX) $(basename $<).tex
	$(DVIPDFMX) $(basename $<).dvi

# for building PDF version of kadai
kadai:  chap02_kadai.pdf chap03_kadai.pdf chap04_kadai.pdf \
		chap05_kadai.pdf chap06_kadai.pdf chap06_kadai.pdf \
		chap07_kadai.pdf chap08_kadai.pdf chap09_kadai.pdf \
		chap10_kadai.pdf

chap%_kadai.pdf:  chap%_kadai.rst
	$(PANDOC) --filter=./pdfilter.py --template $(KADAI_TEMPLATE) $< -o $(basename $<).tex
	$(LATEX) $(basename $<).tex
	$(LATEX) $(basename $<).tex
	$(DVIPDFMX) $(basename $<).dvi
