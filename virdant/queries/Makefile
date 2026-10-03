.PHONY: all pdf svg clean

DOT := dependencies.dot

all: pdf

pdf: dependencies.pdf

svg: dependencies.svg

dependencies.pdf: $(DOT)
	dot -Tpdf $(DOT) -o $@

dependencies.svg: $(DOT)
	dot -Tsvg $(DOT) -o $@

clean:
	rm -f dependencies.pdf dependencies.svg
