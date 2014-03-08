ortho_srcdir=..
orthobe_srcdir=$(ortho_srcdir)/gcc
agcc_objdir=.
agcc_srcdir=$(ortho_srcdir)/gcc
SED=sed
BE=gcc
GNATMAKE=gnatmake
CC=gcc
CXX=g++
COMPILER=$(CXX)
LINKER=$(CXX)

# Modify AGCC_GCCSRC_DIR and AGCC_GCCOBJ_DIR for your environment
AGCC_GCCSRC_DIR:=$(HOME)/Projects/gcc4.8.2/source/gcc-4.8.2/
AGCC_GCCOBJ_DIR:=$(HOME)/Projects/gcc4.8.2/build/

# Supplied by main GCC Makefile, copied here for compatibility with same
GMPLIBS = -L$(AGCC_GCCOBJ_DIR)./gmp/.libs -L$(AGCC_GCCOBJ_DIR)./mpfr/.libs \
  -L$(AGCC_GCCOBJ_DIR)./mpc/src/.libs -lmpc -lmpfr -lgmp
GMPINC = -I$(AGCC_GCCOBJ_DIR)./gmp -I$(AGCC_GCCSRC_DIR)/gmp \
  -I$(AGCC_GCCOBJ_DIR)./mpfr -I$(AGCC_GCCSRC_DIR)/mpfr \
  -I$(AGCC_GCCSRC_DIR)/mpc/src

HOST_LIBS =
ZLIB=-lz

# Override variables in Makefile.conf for your environment
-include $(orthobe_srcdir)/Makefile.conf

all: $(ortho_exec)

ORTHO_BASENAME=ortho_gcc
include $(orthobe_srcdir)/Makefile.inc
include $(ortho_srcdir)/Makefile.inc

LIBBACKTRACE = $(AGCC_GCCOBJ_DIR)/libbacktrace/.libs/libbacktrace.a
LIBDECNUMBER = $(AGCC_GCCOBJ_DIR)/libdecnumber/libdecnumber.a
LIBIBERTY = $(AGCC_GCCOBJ_DIR)/libiberty/libiberty.a
CPPLIB= # Not needed for GHDL

BACKEND = $(AGCC_GCCOBJ_DIR)/gcc/libbackend.a \
         $(AGCC_GCCOBJ_DIR)/gcc/libcommon-target.a

BACKENDLIBS = $(CLOOGLIBS) $(GMPLIBS) $(PLUGINLIBS) $(HOST_LIBS) \
	$(ZLIB)
LIBS =  $(AGCC_GCCOBJ_DIR)/gcc/libcommon.a \
        $(CPPLIB) $(LIBINTL) $(LIBICONV) $(LIBBACKTRACE) \
	$(LIBIBERTY) $(LIBDECNUMBER) $(HOST_LIBS)

$(ortho_exec): $(AGCC_DEPS) $(orthobe_srcdir)/ortho_gcc.ads force
	$(GNATMAKE) -m -o $@ -g -aI$(ortho_srcdir) \
	 -aI$(ortho_srcdir)/gcc $(GNAT_FLAGS) ortho_gcc-main \
	 -bargs -E -largs --LINK=$(LINKER) $(AGCC_OBJS) \
        $(BACKEND) $(LIBS) $(BACKENDLIBS)

clean: agcc-clean
	$(RM) -f *.o *.ali ortho_nodes-main
	$(RM) b~*.ad? *~

distclean: clean agcc-clean


force:

.PHONY: force all clean

