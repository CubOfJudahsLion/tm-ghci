# Converts the TeXmacs file into Markdown
# Requires pandoc-cli and TeXmacs

.ONESHELL:

.PHONY: clean cleanall


# My particular installation uses an AppImage. Change to suit your setup.
TM_CMD := /opt/TeXmacs/TeXmacs-2.1.5-x86_64.AppImage

DOLLAR := $$

../../README.md: README.tex
	pandoc -r latex -w gfm $< -o $@
	sed -r -e 's/<\/?span\b[^>]*>//g'\
	       -e 's/^(`tm-ghci`[[:space:]]*::[[:space:]]*).*${DOLLAR}/<span style="font-size: 20pt">\1![Haskell](img\/haskell.png) \&rarr; ![TeXmacs](img\/texmacs.png)<\/span>/'\
	    -i $@

README.tex: README.tm
	${TM_CMD} -C $< $@
	sed -r -e '/\\documentclass\{/a\\\\usepackage{ulem}'\
	       -e 's/\\strikethrough\{/\\sout{/'\
	       -e 's/\{\\tau\}/{$$\\tau$$}/'\
	       -e '/^\\newcommand\{.*$$/d'\
	       -e '/^\\newenvironment\{.*$$/d'\
	       -e '/^%/d'\
	       -e 's/\\tm(acronym|name|person)\{/\\textsc{/g'\
	       -e 's/\\tmverbatim\{/\\texttt{/g'\
	       -e 's/\\TeXmacs/TeXmacs/g'\
	       -e 's/\\tmem\{/\\emph{/g'\
	       -e 's/\\begin\{tmcode\}\[shell\]/\\begin{verbatim}/g'\
	       -e 's/\\end\{tmcode\}/\\end{verbatim}/g'\
	       -e 's/\\center\{/{/g'\
	    -i $@

clean:
	-rm README.{aux,log,out,tex,tm\~}

cleanall: clean
	-rm ../../README.md

