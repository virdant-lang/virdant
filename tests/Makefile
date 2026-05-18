SUBDIRS := $(wildcard */)

test:
	for d in $(SUBDIRS); do $(MAKE) -C $$d || exit 1; done
