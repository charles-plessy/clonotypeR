html-doc:
	TMPHTML=$$(mktemp --directory) && R CMD INSTALL --library=$$TMPHTML --html . && cp $$TMPHTML/clonotypeR/html/* html && rm --recursive --force $$TMPHTML
	sed -i -e '/.jpg/d' html/00Index.html
