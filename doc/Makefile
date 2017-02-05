all: ebooklify.info ebooklify.pdf


# makeinfo using the first pre-requisite
ebooklify.info: ebooklify.texi
	makeinfo $<

ebooklify.pdf: ebooklify.texi
	texi2pdf $<

clean:
	rm -f *.pdf
	rm -f *.info
	rm -f *.aux *.toc *.log
	rm -f *.cp *.fn *.pg *.ky *.tp *.vr *.vrs
