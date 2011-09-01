all:
	pdflatex twig-gpu.tex
	bibtex twig-gpu
	pdflatex twig-gpu.tex
	pdflatex twig-gpu.tex

clean:
	rm -f *~ *.bbl *.blg *.log *.aux *.synctex.gz twig-gpu.pdf
