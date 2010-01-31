ortho_srcdir=..
orthobe_srcdir=$(ortho_srcdir)/gcc
agcc_objdir=.
agcc_srcdir=$(ortho_srcdir)/gcc
AGCC_GCCSRC_DIR:=$(HOME)/dist/gcc-4.3.4
AGCC_GCCOBJ_DIR:=$(AGCC_GCCSRC_DIR)-objs/
SED=sed
GNATMAKE=gnatmake
CC=gcc

all: $(ortho_exec)

include $(orthobe_srcdir)/Makefile.inc

ORTHO_BASENAME=$(orthobe_srcdir)/ortho_gcc
ORTHO_PACKAGE=Ortho_Gcc

LIBFLAGS=$(HOME)/dist/mpfr-2.3.1/.libs/libmpfr.a $(HOME)/dist/gmp-4.2.2/.libs/libgmp.a
#LIBFLAGS=-lmpfr -lgmp

$(ortho_exec): $(AGCC_DEPS) $(ORTHO_BASENAME).ads force
	$(GNATMAKE) -m -o $@ -g -aI$(ortho_srcdir) \
	 -aI$(ortho_srcdir)/gcc $(GNAT_FLAGS) ortho_gcc-main \
	 -bargs -E -largs $(AGCC_OBJS) \
	  $(AGCC_GCCOBJ_DIR)libcpp/libcpp.a \
	  $(AGCC_GCCOBJ_DIR)libiberty/libiberty.a \
	  $(AGCC_GCCOBJ_DIR)libdecnumber/libdecnumber.a \
	  $(LIBFLAGS) #-static

clean: agcc-clean
	$(RM) -f *.o *.ali ortho_nodes-main
	$(RM) b~*.ad? *~

distclean: clean agcc-clean


force:

.PHONY: force all clean

