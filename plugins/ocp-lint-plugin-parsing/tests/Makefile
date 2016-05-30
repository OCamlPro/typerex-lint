
PLUGIN=ocp-lint-plugin-parsing
OBUILD=../../../_obuild
LINT=$(OBUILD)/ocp-lint/ocp-lint.asm --load-plugins $(OBUILD)/$(PLUGIN)/$(PLUGIN).cmxs

all:
	rm -rf _obuild _olint
	ocp-build init
	ocp-build ocp-lint-plugin-parsing-test
	$(LINT) --init
	$(LINT) --path . > test.current 2>&1 
	diff test.reference test.current && echo TEST OK || echo TEST FAILED
