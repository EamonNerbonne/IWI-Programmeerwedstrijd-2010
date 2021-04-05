REC_TARGETS = all build check clean distclean

SUBDIRS = $(shell ls */Makefile | sed s@/.*@@ | grep -v example)

$(REC_TARGETS): %:
	@for dir in $(SUBDIRS); do \
	echo; \
	echo "Building problem: $$dir"; \
	$(MAKE) -C $$dir $@ || ( \
		echo; \
		echo "==================================================================================="; \
		echo "== FAILED TO BUILD $$dir"; \
		echo "== Go fix this!"; \
		echo "==================================================================================="; \
		echo; \
		echo; \
		exit 1; \
	); \
	done
