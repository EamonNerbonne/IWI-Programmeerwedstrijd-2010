REC_TARGETS = all build check clean distclean
SUBDIRS = impl input
$(REC_TARGETS): %:
	@for dir in $(SUBDIRS) ; do $(MAKE) -C $$dir $@ || exit 1 ; done
