# Inspired from https://raw.github.com/yihui/knitr/7eb34104d5410645f5c0fa35b4f7906e2bcc1cb8/inst/doc/Makefile

all: clonotypeR.pdf

clean:
	rm -rf *.tex *.bbl *.blg *.aux *.out *.log *.spl *tikzDictionary *.md figure/ *.html

%.pdf: %.md
	pandoc -o $*.pdf $*.md

%.md: %.Rmd
	/usr/bin/Rscript -e "knitr::knit2html('$*.Rmd')"
	rm $*.html
