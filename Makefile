EMACS ?= emacs # set EMACS if not already defined.

.PHONY: clean doc
all: compile doc

doc:
	$(MAKE) -C doc/

compile:
	cask exec $(EMACS) -batch --eval="(add-to-list 'load-path \".\")" \
	--eval "(mapc #'byte-compile-file (list \"ebookify-doc.el\" \"ebookify-mongo.el\" \"ebookify-mode.el\"))"

clean:
	rm -rf *.elc
	$(MAKE) -C doc/ clean
