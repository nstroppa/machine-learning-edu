%.pdf: %.tex 
	pdflatex $*
	pdflatex $*

linear.pdf: linear.R
	R --no-save < linear.R 
gradient.pdf: gradient.R
	R --no-save < gradient.R
clean:
	-rm *.aux *.log *.nav *.out *.snm *.toc
