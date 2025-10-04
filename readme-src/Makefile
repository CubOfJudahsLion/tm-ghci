# Commands to create README.md and README.pdf

DOC_SOURCE := README.tex
IMG_SOURCES := haskell.png texmacs.png texmacs-text.png
MAKE_SOURCES := Makefile
SOURCES := $(DOC_SOURCE) $(IMG_SOURCES) $(MAKE_SOURCES)

TARGET_MD := ../README.md
TARGET_PDF := ../README.pdf
TARGETS := $(TARGET_MD) $(TARGET_PDF)

.PHONY: docs clean cleanall vartest

.ONESHELL:

docs: $(TARGETS)

vartest:
	@echo $$'\e[1;36mDOC_SOURCE :\e[0m' $(DOC_SOURCE)
	@echo $$'\e[1;36mIMG_SOURCES:\e[0m' $(IMG_SOURCES)
	@echo $$'\e[1;36mSOURCES    :\e[0m' $(SOURCES)
	@echo $$'\e[1;36mTARGET_MD  :\e[0m' $(TARGET_MD)
	@echo $$'\e[1;36mTARGET_PDF :\e[0m' $(TARGET_PDF)
	@echo $$'\e[1;36mTARGETS    :\e[0m' $(TARGETS)

$(TARGET_MD): $(SOURCES)
	echo "# <span style=\"font-family: monospace\">tm-ghci &#x2237; \
	<img src=\"readme-src/haskell.png\" width=\"36\" height=\"32\"> &#x2192; \
	<img src=\"readme-src/texmacs.png\" width=\"36\" height=\"32\"></span>" > $@
	pandoc -f latex -t gfm $< | \
	sed -r -e \
	  '/^#/s/^(#+)/#\1/
	  /\bspan\b/{
	    s/<span class="smallcaps">/<span style="font-variant: small-caps">/g
	    s/<span class="sans-serif">/<span style="font-family: serif">/g
	  }
	  /\bsrc="texmacs-text.png"/ {
	    s/\bwidth="[^"]+"/width="60"/
	    s/\bheight="[^"]+"/height="14"/
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

