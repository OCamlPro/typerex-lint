TEX = pdflatex -shell-escape -interaction=nonstopmode -file-line-error

all : ocp-lint-web.pdf

view :
	 open ocp-lint-web.pdf

ocp-lint-web.pdf :
	$(TEX) ocp-lint-web.tex

clean:
	rm -f *.log
	rm -f *.aux
	rm -f *.out
	rm -f *.toc
	rm -f *.pdf
