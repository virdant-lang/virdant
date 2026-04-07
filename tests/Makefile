SUBDIRS := $(wildcard */)
test_all:
	for d in $(SUBDIRS); do $(MAKE) -C $$d || exit 1; done

.PHONY: sim
