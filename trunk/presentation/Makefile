all: session-1a.pdf session-1b.pdf session-2.pdf session-3.pdf session-4.pdf

%.pdf: %.tex
	pdflatex $*
	pdflatex $*
clean:
	-rm *.aux *.log *.nav *.out *.snm *.toc
