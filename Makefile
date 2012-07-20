html-doc:
	TMPHTML=$$(mktemp --directory) && R CMD INSTALL --library=$$TMPHTML --html . && cp $$TMPHTML/clonotypeR/html/* html && rm --recursive --force $$TMPHTML
	sed -i -e '/.jpg/d' -e's|</body>|<hr /><p style="text-align:center">Back to <a href="..">home page</a>.</p></body>|' html/00Index.html
