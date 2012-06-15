DOC=twig

all:
	pdflatex $(DOC).tex
	bibtex $(DOC)
	pdflatex $(DOC).tex
	pdflatex $(DOC).tex

clean:
	rm -f *~ *.toc *.bbl *.blg *.log *.aux *.synctex.gz *.lof *.lot
	rm -f $(DOC).pdf
