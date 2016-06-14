NAME = typerex-lint
DEPS = $(wildcard *.tex)

all: *.tex
	rubber --pdf $(NAME)

clean:
	-rm -f $(NAME).log $(NAME).toc $(NAME).blg $(NAME).bbl $(NAME).aux
	-rm -f *~

bib: $(NAME).bbl

.phony: clean bib
