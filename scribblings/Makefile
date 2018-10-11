.PHONY: all clean

all: $(wildcard *.scrbl)
	scribble riposte.scrbl

clean:
	rm -Rf compiled
	find . -mindepth 1 -maxdepth 1 -type f -name '*~' -delete
	find . -mindepth 1 -maxdepth 1 -type d | xargs -J '{}' $(MAKE) -C {} clean
