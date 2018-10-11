.PHONY: all clean

all: $(wildcard *.scrbl)
	scribble riposte.scrbl

clean:
	rm -Rf compiled
	rm -Rf doc
	find . -mindepth 1 -maxdepth 1 -type f  \( -name '*.css' -o -name '*.js' -o -name '*.html' -o -name '*~' \) -delete
