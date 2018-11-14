.PHONY: all clean check

all: check riposte

clean:
	rm -Rf compiled
	rm -f riposte
	rm -Rf doc
	# remove scribble remains, if any:
	find . -mindepth 1 -maxdepth 1 -type f -name '*.css' -delete
	find . -mindepth 1 -maxdepth 1 -type f -name '*.js' -delete
	find . -mindepth 1 -maxdepth 1 -type f -name '*.html' -delete
	# kill Emacs backup files
	find . -mindepth 1 -maxdepth 1 -type f -name '*~' -delete
	# clean subdirectories (except .git)
	find . -mindepth 1 -maxdepth 1 -type d ! -name '.git' -exec $(MAKE) -C {} clean ';'

riposte: $(wildcard *.rkt)
	raco exe riposte.rkt

check:
	raco test *.rkt
	raco setup --check-pkg-deps --fix-pkg-deps --unused-pkg-deps --pkgs riposte
