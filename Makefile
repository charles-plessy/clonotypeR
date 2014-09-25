html-doc:
	TMPHTML=$$(mktemp --directory) && R CMD INSTALL --library=$$TMPHTML --html . && cp $$TMPHTML/clonotypeR/html/* html_doc && rm --recursive --force $$TMPHTML
	sed -i -e '/.jpg/d' -e's|</body>|<hr /><p style="text-align:center">Back to <a href="..">home page</a>.</p></body>|' html_doc/00Index.html
	/usr/bin/Rscript -e "knitr::knit2html('vignettes/clonotypeR.Rmd')"
	mv clonotypeR.html html_doc
	rm clonotypeR.md

manpage: doc/clonotyper.mdwn
	pandoc -s -w man doc/clonotyper.mdwn -o scripts/clonotyper.1

r-check:
	$(RM) --recursive ..Rcheck
	R CMD check .

r-build:
	SOURCEDIR=$$(pwd) && cd .. && R CMD build $$SOURCEDIR
