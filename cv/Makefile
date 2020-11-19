SOURCES := $(shell find . -type f -and \( -name '*.tex' -or -name '*.bib' -or -name '*.cls' \))
MAIN ?= academic
MAINS := academic
VIEW := open -a Skim

default: build

.PHONY: build
build: $(MAIN).pdf

.PHONY: watch
watch:
	@fswatch -o $(SOURCES) | xargs -n1 -I{} make

.PHONY: clean
clean:
	@latexmk -C $(MAINS)
	@rm -f *.run.xml *.bbl
	@rm -rf auto/

$(MAIN).pdf: $(SOURCES)
	@latexmk -pdflua $(MAIN).tex -halt-on-error

view: $(MAIN).pdf
	@open -a Skim $(MAIN).pdf


