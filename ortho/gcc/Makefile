ortho_srcdir=..
orthobe_srcdir=$(ortho_srcdir)/gcc
agcc_objdir=.
agcc_srcdir=$(ortho_srcdir)/gcc
AGCC_GCCSRC_DIR:=$(HOME)/dist/gcc-4.0.2
AGCC_GCCOBJ_DIR:=$(AGCC_GCCSRC_DIR)-objs
SED=sed

all: $(ortho_exec)

include $(orthobe_srcdir)/Makefile.inc

ORTHO_BASENAME=$(orthobe_srcdir)/ortho_gcc
ORTHO_PACKAGE=Ortho_Gcc


$(ortho_exec): $(AGCC_DEPS) $(ORTHO_BASENAME).ads force
	gnatmake -m -o $@ -g -aI$(ortho_srcdir) \
	 -aI$(ortho_srcdir)/gcc $(GNAT_FLAGS) ortho_gcc-main \
	 -bargs -E -largs $(AGCC_OBJS) #-static

clean: agcc-clean
	$(RM) -f *.o *.ali ortho_nodes-main
	$(RM) b~*.ad? *~

distclean: clean agcc-clean


force:

.PHONY: force all clean

