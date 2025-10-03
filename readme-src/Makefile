# Commands to create README.md and README.pdf

DOC_SOURCE := README.tex
IMG_SOURCES := haskell.png texmacs.png texmacs-text.png
MAKE_SOURCES := Makefile
SOURCES := $(DOC_SOURCE) $(IMG_SOURCES) $(MAKE_SOURCES)

TARGET_MD := ../README.md
TARGET_PDF := ../README.pdf
TARGETS := $(TARGET_MD) $(TARGET_PDF)

.PHONY: docs clean cleanall testvars

.ONESHELL:

docs: $(TARGETS)

testvars:
	@echo DOC_SOURCE : $(DOC_SOURCE)
	@echo IMG_SOURCES: $(IMG_SOURCES)
	@echo SOURCES    : $(SOURCES)
	@echo TARGET_MD  : $(TARGET_MD)
	@echo TARGET_PDF : $(TARGET_PDF)
	@echo TARGETS    : $(TARGETS)

$(TARGET_MD): $(SOURCES)
	echo "<span style=\"font-family: monospace; font-size: 16pt\">tm-ghci &#x2237;
		<img src=\"readme-src/haskell.png\" width=\"24\" height=\"24\"> &#x2192;
		<img src=\"readme-src/texmacs.png\" width=\"24\" height=\"24\"></span><br><br><br><br>" \
		> $@
	pandoc -f latex -t gfm $< | \
	sed -r -e \
	  '/\bspan\b/{
	    s/<span class="smallcaps">/<span style="font-variant: small-caps">/g
	    s/<span class="sans-serif">/<span style="font-family: serif">/g
	  }
	  /<img src=/s/"([^.]+\.png)"/"readme-src\/\1"/g
	  /`make deploy`/s/^.*$$/```shell\n    make deploy\n```/' \
	>> $@

$(TARGET_PDF): $(SOURCES)
	pdflatex $<
	mv $(shell basename $@) $@

clean:
	find . \( -iname \*.out -o -iname \*.log -o -iname *.aux \) -delete

cleanall: clean
	rm -f $(TARGETS)

