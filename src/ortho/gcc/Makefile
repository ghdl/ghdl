ortho_srcdir=..
orthobe_srcdir=$(ortho_srcdir)/gcc
ortho_lang_c=ortho-lang-7.c
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
AGCC_GCCSRC_DIR:=$(HOME)/Projects/gcc4.9.2/source/gcc-4.9.2/
AGCC_GCCOBJ_DIR:=$(HOME)/Projects/gcc4.9.2/build/

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
include $(ortho_srcdir)/Makefile.inc

AGCC_INC_FLAGS=-I$(AGCC_GCCOBJ_DIR)/gcc -I$(AGCC_GCCSRC_DIR)/include \
 -I$(AGCC_GCCSRC_DIR)/gcc -I$(AGCC_GCCSRC_DIR)/gcc/config \
 -I$(AGCC_GCCSRC_DIR)/libcpp/include $(GMPINC)
AGCC_CFLAGS=-DIN_GCC_FRONTEND -Wall -DIN_GCC -DHAVE_CONFIG_H $(AGCC_INC_FLAGS)

ortho-lang.o: $(agcc_srcdir)/$(ortho_lang_c) \
 $(AGCC_GCCOBJ_DIR)gcc/gtype-vhdl.h \
 $(AGCC_GCCOBJ_DIR)gcc/gt-vhdl-ortho-lang.h
	$(COMPILER) -c -o $@ $< $(AGCC_CFLAGS) $(CXXFLAGS) $(INCLUDES)

AGCC_LOCAL_OBJS=ortho-lang.o

AGCC_DEPS := $(AGCC_LOCAL_OBJS)
AGCC_OBJS := $(AGCC_LOCAL_OBJS) \
	$(AGCC_GCCOBJ_DIR)gcc/attribs.o \
	$(AGCC_GCCOBJ_DIR)libcpp/libcpp.a \
	$(AGCC_GCCOBJ_DIR)libiberty/libiberty.a

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
	$(GNATMAKE) -o $@ -aI$(ortho_srcdir) \
	 -aI$(ortho_srcdir)/gcc $(GNATFLAGS) ortho_gcc-main \
	 -bargs -E -largs --LINK=$(LINKER) $(AGCC_OBJS) \
        $(BACKEND) $(LIBS) $(BACKENDLIBS) $(LDFLAGS)

agcc-clean: force
	$(RM) -f $(agcc_objdir)/*.o
	$(RM) -f $(agcc_srcdir)/*~

clean: agcc-clean
	$(RM) -f *.o *.ali ortho_nodes-main
	$(RM) b~*.ad? *~

distclean: clean agcc-clean


force:

.PHONY: force all clean agcc-clean
