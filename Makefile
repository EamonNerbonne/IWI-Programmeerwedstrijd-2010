
SUBDIRS = problems contests tools

all: $(SUBDIRS)
clean:
	rm -f inout

.PHONY: $(SUBDIRS)

$(SUBDIRS):
	@echo
	@echo
	@echo "==================== $@ ===================="
	@echo
	@$(MAKE) -C $@ $* || echo "Ga dingen fixen!"

# Input/output Directories

all: inout-iwi

#inout-%: rec-all
inout-%: problems
	@echo
	@echo
	@echo "==================== inout/$* ===================="
	@echo
	rm -rf inout/$*
	mkdir -p inout/$*
	perl tools/copy-problems.pl $*

inoutzip-%: inout-%
	cd inout/$*;zip -r example.zip   example
	cd inout/$*;zip -r testdata.zip  testdata
	cd inout/$*;zip -r solutions.zip solutions
	cp contests/$*.pdf inout/$*/problems.pdf
