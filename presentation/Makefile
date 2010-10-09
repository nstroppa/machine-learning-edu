all: session-1a.pdf session-1b.pdf session-2.pdf session-3.pdf session-4.pdf

%.pdf: %.tex 
	pdflatex $*
	pdflatex $*

session-1b.pdf: session-1b.tex linear.pdf
	pdflatex session-1b.tex

linear.pdf: linear.R
	R --no-save < linear.R 

clean:
	-rm *.aux *.log *.nav *.out *.snm *.toc
