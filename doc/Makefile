all: ebookify.info ebookify.pdf


# makeinfo using the first pre-requisite
ebookify.info: ebookify.texi
	makeinfo $<

ebookify.pdf: ebookify.texi
	texi2pdf $<

clean:
	rm -f *.pdf
	rm -f *.info
	rm -f *.aux *.toc *.log
	rm -f *.cp *.fn *.pg *.ky *.tp *.vr *.vrs
