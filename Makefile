.PHONY: all clean

all: riposte

clean:
	rm -Rf compiled
	rm -f riposte
	rm -Rf doc
	find . -mindepth 1 -maxdepth 1 -type f -name '*~' -delete
	find . -mindepth 1 -maxdepth 1 -type d ! -name '.git' -exec $(MAKE) -C {} clean ';'

riposte: $(wildcard *.rkt)
	raco exe riposte.rkt

check:
	raco test *.rkt
