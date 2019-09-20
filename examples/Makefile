.PHONY: check clean

clean:
	find . -mindepth 1 -maxdepth 1 -type f -name '*~' -delete
	find . -mindepth 1 -maxdepth 1 -type d -exec $(MAKE) -C {} clean

check:
	find . -type f '*.rip' -exec
