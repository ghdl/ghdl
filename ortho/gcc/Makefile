ortho_srcdir=..
orthobe_srcdir=$(ortho_srcdir)/gcc
agcc_objdir=.
agcc_srcdir=$(ortho_srcdir)/gcc
# Modify AGCC_GCCSRC_DIR and AGCC_GCCOBJ_DIR for your environment
AGCC_GCCSRC_DIR:=$(HOME)/Projects/gcc4.8.2/source/gcc-4.8.2/
AGCC_GCCOBJ_DIR:=$(HOME)/Projects/gcc4.8.2/build/
SED=sed
GNATMAKE=gnatmake
CC=gcc
COMPILER=g++

all: $(ortho_exec)

include $(orthobe_srcdir)/Makefile.inc

ORTHO_BASENAME=$(orthobe_srcdir)/ortho_gcc
ORTHO_PACKAGE=Ortho_Gcc

# Supplied by main GCC Makefile, copied here for compatibility with same
GMPLIBS = -L$(AGCC_GCCOBJ_DIR)./gmp/.libs -L$(AGCC_GCCOBJ_DIR)./mpfr/.libs -L$(AGCC_GCCOBJ_DIR)./mpc/src/.libs -lmpc -lmpfr -lgmp
GMPINC = -I$(AGCC_GCCOBJ_DIR)./gmp -I$(AGCC_GCCSRC_DIR)/gmp -I$(AGCC_GCCOBJ_DIR)./mpfr -I$(AGCC_GCCSRC_DIR)/mpfr -I$(AGCC_GCCSRC_DIR)/mpc/src 

LIBFLAGS=$(GMPLIBS) -lz -ldl -lstdc++


$(ortho_exec): $(AGCC_DEPS) $(ORTHO_BASENAME).ads force
	$(GNATMAKE) -m -o $@ -g -aI$(ortho_srcdir) \
	 -aI$(ortho_srcdir)/gcc $(GNAT_FLAGS) ortho_gcc-main \
	 -bargs -E -largs $(AGCC_OBJS) \
	  $(AGCC_GCCOBJ_DIR)gcc/libcommon-target.a \
	  $(AGCC_GCCOBJ_DIR)gcc/libcommon.a \
	  $(AGCC_GCCOBJ_DIR)libbacktrace/.libs/libbacktrace.a \
	  $(AGCC_GCCOBJ_DIR)libdecnumber/libdecnumber.a \
	  $(LIBFLAGS) #-static

clean: agcc-clean
	$(RM) -f *.o *.ali ortho_nodes-main
	$(RM) b~*.ad? *~

distclean: clean agcc-clean


force:

.PHONY: force all clean

