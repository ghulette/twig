DOC=twig-language

all:
	pdflatex $(DOC).tex
	bibtex $(DOC)
	pdflatex $(DOC).tex
	pdflatex $(DOC).tex

clean:
	rm -f *~ *.bbl *.blg *.log *.aux *.synctex.gz $(DOC).pdf
