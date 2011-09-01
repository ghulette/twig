all:
	pdflatex twig-language.tex
	bibtex twig-language
	pdflatex twig-language.tex
	pdflatex twig-language.tex

clean:
	rm -f *~ *.bbl *.blg *.log *.aux *.synctex.gz twig-language.pdf
