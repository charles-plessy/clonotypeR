html-doc:
	TMPHTML=$$(mktemp --directory) && R CMD INSTALL --library=$$TMPHTML --html . && cp $$TMPHTML/clonotypeR/html/* html_doc && rm --recursive --force $$TMPHTML
	sed -i -e '/.jpg/d' -e's|</body>|<hr /><p style="text-align:center">Back to <a href="..">home page</a>.</p></body>|' html_doc/00Index.html

manpage: doc/clonotyper.mdwn
	pandoc -s -w man doc/clonotyper.mdwn -o scripts/clonotyper.1

r-check:
	$(RM) --recursive ..Rcheck
	R CMD check .

r-build:
	SOURCEDIR=$$(pwd) && cd .. && R CMD build $$SOURCEDIR
