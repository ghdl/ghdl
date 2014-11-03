#  -*- Makefile -*- for the GHDL drivers.
#  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
#
#  GHDL is free software; you can redistribute it and/or modify it under
#  the terms of the GNU General Public License as published by the Free
#  Software Foundation; either version 2, or (at your option) any later
#  version.
#
#  GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
#  WARRANTY; without even the implied warranty of MERCHANTABILITY or
#  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
#  for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with GCC; see the file COPYING.  If not, write to the Free
#  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
#  02111-1307, USA.
GNATFLAGS=-gnaty3befhkmr -gnata -gnatwael -aI../.. -aI.. -aI../../psl -aI../grt -aO.. -g -gnatf -gnat05
GRT_FLAGS=-g
LIB_CFLAGS=-g -O2
GNATMAKE=gnatmake
CC=gcc

# Optimize, do not forget to use MODE=--genfast for iirs.adb.
#GNATFLAGS+=-O -gnatn
#GRT_FLAGS+=-O

# Profiling.
#GNATFLAGS+=-pg -gnatn -O
#GRT_FLAGS+=-pg -O

# Coverage
#GNATFLAGS+=-fprofile-arcs -ftest-coverage

GNAT_BARGS=-bargs -E

LLVM_CONFIG=llvm-config

#GNAT_LARGS= -static
all: ghdl_mcode

target=i686-pc-linux-gnu
#target=x86_64-pc-linux-gnu
#target=i686-apple-darwin
#target=x86_64-apple-darwin
#target=i386-pc-mingw32
GRTSRCDIR=../grt
include $(GRTSRCDIR)/Makefile.inc

ifeq ($(filter-out i%86 linux,$(arch) $(osys)),)
  ORTHO_X86_FLAGS=Flags_Linux
endif
ifeq ($(filter-out i%86 darwin%,$(arch) $(osys)),)
  ORTHO_X86_FLAGS=Flags_Macosx
endif
ifeq ($(filter-out i%86 mingw32%,$(arch) $(osys)),)
  ORTHO_X86_FLAGS=Flags_Windows
endif
ifdef ORTHO_X86_FLAGS
  ORTHO_DEPS=ortho_code-x86-flags.ads
endif

ortho_code-x86-flags.ads:
	echo "with Ortho_Code.X86.$(ORTHO_X86_FLAGS);" > $@
	echo "package Ortho_Code.X86.Flags renames Ortho_Code.X86.$(ORTHO_X86_FLAGS);" >> $@

ghdl_mcode: GRT_FLAGS+=-DWITH_GNAT_RUN_TIME
ghdl_mcode: default_pathes.ads $(GRT_ADD_OBJS) $(ORTHO_DEPS) memsegs_c.o chkstk.o force
	$(GNATMAKE) -o $@ -aI../../ortho/mcode -aI../../ortho $(GNATFLAGS) ghdl_jit.adb $(GNAT_BARGS) -largs memsegs_c.o chkstk.o $(GNAT_LARGS) $(GRT_ADD_OBJS) $(subst @,$(GRTSRCDIR),$(GRT_EXTRA_LIB))

memsegs_c.o: ../../ortho/mcode/memsegs_c.c
	$(CC) -c -g -o $@ $<

ghdl_llvm_jit: GRT_FLAGS+=-DWITH_GNAT_RUN_TIME
ghdl_llvm_jit: default_pathes.ads $(GRT_ADD_OBJS) $(ORTHO_DEPS) llvm-cbindings.o force
	$(GNATMAKE) -o $@ -aI../../ortho/llvm -aI../../ortho $(GNATFLAGS) ghdl_jit.adb $(GNAT_BARGS) -largs llvm-cbindings.o $(GNAT_LARGS) $(GRT_ADD_OBJS) $(subst @,$(GRTSRCDIR),$(GRT_EXTRA_LIB)) `$(LLVM_CONFIG) --ldflags --libs --system-libs` -lc++

llvm-cbindings.o: ../../ortho/llvm/llvm-cbindings.cpp
	$(CXX) -c -m64 `$(LLVM_CONFIG) --includedir --cxxflags` -g -o $@ $<

ghdl_simul: default_pathes.ads $(GRT_ADD_OBJS) force
	$(GNATMAKE) -aI../../simulate $(GNATFLAGS) ghdl_simul $(GNAT_BARGS) -largs $(GNAT_LARGS) $(GRT_ADD_OBJS) $(subst @,$(GRTSRCDIR),$(GRT_EXTRA_LIB))

ghdl_gcc: default_pathes.ads force
	$(GNATMAKE) $(GNATFLAGS) ghdl_gcc $(GNAT_BARGS) -largs $(GNAT_LARGS)

ghdl_llvm: default_pathes.ads force
	$(GNATMAKE) $(GNATFLAGS) ghdl_llvm $(GNAT_BARGS) -largs $(GNAT_LARGS)

default_pathes.ads: default_pathes.ads.in Makefile
	curdir=`cd ..; pwd`; \
	sed -e "s%@COMPILER_GCC@%$$curdir/ghdl1-gcc%" \
	 -e "s%@COMPILER_DEBUG@%$$curdir/ghdl1-debug%" \
	 -e "s%@COMPILER_MCODE@%$$curdir/ghdl1-mcode%" \
	 -e "s%@COMPILER_LLVM@%$$curdir/ghdl1-llvm%" \
	 -e "s%@POST_PROCESSOR@%$$curdir/../ortho/oread/oread-gcc%" \
	 -e "s%@INSTALL_PREFIX@%%" \
	 -e "s%@LIB_PREFIX@%$$curdir/lib/%" < $< > $@

bootstrap.old: force
	$(RM) ../../libraries/std-obj87.cf
	$(MAKE) -C ../../libraries EXT=obj \
	  ANALYSE="$(PWD)/ghdl -a -g" std-obj87.cf
	$(RM) ../../libraries/std-obj93.cf
	$(MAKE) -C ../../libraries EXT=obj \
	  ANALYSE="$(PWD)/ghdl -a -g" std-obj93.cf

LIB87_DIR:=../lib/v87
LIB93_DIR:=../lib/v93
LIB08_DIR:=../lib/v08

LIBSRC_DIR:=../../libraries
REL_DIR:=../..
GHDL=ghdl
ANALYZE:=../../../ghdldrv/$(GHDL) -a $(LIB_CFLAGS)
LN=ln -s
CP=cp

$(LIB87_DIR) $(LIB93_DIR) $(LIB08_DIR):
	[ -d ../lib ] || mkdir ../lib
	[ -d $@ ] || mkdir $@

include ../../libraries/Makefile.inc

GHDL1=../ghdl1-gcc
$(LIB93_DIR)/std/std_standard.o: $(GHDL1)
ifeq ($(GHDL),ghdl_llvm)
	$(GHDL1) --std=93 -quiet $(LIB_CFLAGS)  -c -o $@ --compile-standard
else
	$(GHDL1) --std=93 -quiet $(LIB_CFLAGS)  -o std_standard.s \
	 --compile-standard
	$(CC) -c -o $@ std_standard.s
	$(RM) std_standard.s
endif

$(LIB87_DIR)/std/std_standard.o: $(GHDL1)
ifeq ($(GHDL),ghdl_llvm)
	$(GHDL1) --std=87 -quiet $(LIB_CFLAGS)  -c -o $@ --compile-standard
else
	$(GHDL1) --std=87 -quiet $(LIB_CFLAGS) -o std_standard.s \
	 --compile-standard
	$(CC) -c -o $@ std_standard.s
	$(RM) std_standard.s
endif

$(LIB08_DIR)/std/std_standard.o: $(GHDL1)
ifeq ($(GHDL),ghdl_llvm)
	$(GHDL1) --std=08 -quiet $(LIB_CFLAGS)  -c -o $@ --compile-standard
else
	$(GHDL1) --std=08 -quiet $(LIB_CFLAGS) -o std_standard.s \
	 --compile-standard
	$(CC) -c -o $@ std_standard.s
	$(RM) std_standard.s
endif

install.v93: std.v93 ieee.v93 synopsys.v93 mentor.v93
install.v87: std.v87 ieee.v87 synopsys.v87
install.v08: std.v08 ieee.v08

install.standard: $(LIB93_DIR)/std/std_standard.o \
 $(LIB87_DIR)/std/std_standard.o \
 $(LIB08_DIR)/std/std_standard.o

grt.links:
	cd ../lib; ln -sf $(GRTSRCDIR)/grt.lst .; ln -sf $(GRTSRCDIR)/libgrt.a .; ln -sf $(GRTSRCDIR)/grt.ver .

install.all: install.v87 install.v93 install.v08

install.gcc:
	$(MAKE) GHDL=ghdl_gcc install.all
	$(MAKE) GHDL1=../ghdl1-gcc install.standard

install.mcode:
	$(MAKE) GHDL=ghdl_mcode install.all

install.simul:
	$(MAKE) GHDL=ghdl_simul install.all

install.llvm:
	$(MAKE) GHDL=ghdl_llvm install.all
	$(MAKE) GHDL1=../ghdl1-llvm install.standard

clean: force
	$(RM) -f *.o *.ali ghdl_gcc ghdl_mcode ghdl_llvm ghdl_llvm_jit
	$(RM) -f b~*.ad? *~ default_pathes.ads ghdl_simul
	$(RM) -rf ../lib

clean-c: force
	$(RM) -f memsegs_c.o chkstk.o linux.o times.o grt-cbinding.o grt-cvpi.o

force:

.PHONY: force clean
