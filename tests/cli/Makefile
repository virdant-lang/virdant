# Unique work directory per make invocation: tests/tmp.<random suffix>
WORKDIR := $(CURDIR)/../tmp.$(shell echo $$RANDOM$$RANDOM | md5sum | cut -c1-6)

test:
	mkdir -p $(WORKDIR)
	bash new_check.sh $(WORKDIR) > /dev/null
	bash new_run_flags.sh $(WORKDIR) > /dev/null
	bash new_compile.sh $(WORKDIR) > /dev/null
	bash new_bitstream.sh $(WORKDIR) > /dev/null 2>&1
	bash new_tutor.sh $(WORKDIR) > /dev/null 2>&1
	rm -rf $(WORKDIR)
