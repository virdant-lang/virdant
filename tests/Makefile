VIR       ?= $(abspath ..)/target/release/vir
FILECHECK ?= $(abspath ..)/target/release/filecheck

SUBDIRS := $(wildcard */)

test:
	for d in $(SUBDIRS); do \
		echo "--------------------------------------------------------------------------------"; \
		echo "TEST: tests/$$d"; \
		echo "--------------------------------------------------------------------------------"; \
		$(MAKE) -C $$d VIR=$(VIR) FILECHECK=$(FILECHECK) || exit 1; \
	done
