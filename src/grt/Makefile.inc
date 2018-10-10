#  -*- Makefile -*- for the GHDL Run Time library.
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

# Variables used:
# AR: ar command
# RM
# CC
# GNATMAKE: the GNAT compiler (gnatmake)
# GHDL1: the ghdl compiler
# GRT_RANLIB: the ranlib tool for the grt library.
# grt_libdir: the place to put grt.
# GRTSRCDIR: the source directory of grt.
# target: GCC target
# GRT_FLAGS: common (Ada + C + asm) compilation flags.
# GRT_ADAFLAGS: compilation flags for Ada
#
# LIBBACKTRACE: if set, path to libbacktrace.a (from gcc)

# Convert the target variable into a space separated list of architecture,
# manufacturer, and operating system and assign each of those to its own
# variable.

target1:=$(subst -gnu,,$(target))
targ:=$(subst -, ,$(target1))
arch:=$(word 1,$(targ))
ifeq ($(words $(targ)),2)
  osys:=$(word 2,$(targ))
else
  osys:=$(word 3,$(targ))
endif

GRT_ELF_OPTS:=-Wl,--version-script=@/grt.ver -Wl,--export-dynamic

# Set target files.
ifeq ($(filter-out mingw32 mingw64,$(osys)),)
  GRT_TARGET_OBJS=win32.o clock.o
  GRT_EXTRA_LIB=-ldbghelp
else
  GRT_TARGET_OBJS=jumps.o times.o
  ifeq ($(filter-out linux%,$(osys)),)
    GRT_EXTRA_LIB=-ldl -lm $(GRT_ELF_OPTS)
  endif
  ifeq ($(filter-out netbsd freebsd% dragonfly%,$(osys)),)
    GRT_EXTRA_LIB=-lm $(GRT_ELF_OPTS)
  endif
  ifeq ($(filter-out solaris%,$(osys)),)
    GRT_EXTRA_LIB=-ldl -lm
  endif
  ifeq ($(filter-out darwin%,$(osys)),)
    GRT_EXTRA_LIB=
  endif
endif

# Object files (from C) for fst
GRT_FST_OBJS := fstapi.o lz4.o fastlz.o

# Additionnal object files (C or asm files).
GRT_BASE_C_OBJS:=$(GRT_TARGET_OBJS) grt-cbinding.o
GRT_ADD_OBJS:=$(GRT_BASE_C_OBJS) grt-cvpi.o $(GRT_FST_OBJS)
GRT_ADD_SHOBJS:=$(patsubst %, pic/%, $(GRT_BASE_C_OBJS))

# Source files create by grt.
GRT_SRC_DEPS:=grt-backtraces-impl.ads

#GRT_USE_PTHREADS=y
ifeq ($(GRT_USE_PTHREADS),y)
 GRT_CFLAGS+=-DUSE_THREADS
 GRT_ADD_OBJS+=grt-cthreads.o
 GRT_EXTRA_LIB+=-lpthread
endif

GRT_LIBBACKTRACE=
ifneq ($(LIBBACKTRACE),)
  GRT_LIBBACKTRACE=libbacktrace.a
endif

# MSYS2 tries to be clever with paths in command line: it replaces msys2 paths
# by Windows paths.  So for example, /bin/ls becomes c:/mingw64/bin/ls
# However, this is not always obvious to detect paths. In our case, msys2
# transformed -aIc:/dir to -aIc:c:/dir, which was not understood by gnatmake.
# So tell MSYS2 not try transform paths that follow -aI or -gnatec
MSYS2_ARG=MSYS2_ARG_CONV_EXCL="-aI;-gnatec"

# Configuration pragmas.
# -gnatdY is required to force the use of __gnat_last_chance_handler.
GRT_PRAGMA_FLAG=-gnatec$(GRTSRCDIR)/grt.adc -gnatdY

# Rule to compile an Ada file.
GRT_ADACOMPILE=$(GNATMAKE) -u -c $(GRT_FLAGS) $(GRT_PRAGMA_FLAG)

grt-all: $(libdirsuffix)/libgrt.a $(libdirsuffix)/$(GRT_LIBBACKTRACE) $(libdirsuffix)/grt.lst $(libdirsuffix)/grt.ver

$(libdirsuffix)/libgrt.a: $(GRT_ADD_OBJS) grt/run-bind.o grt/main.o grt/grt-files
	$(RM) -f $@
	$(AR) rcv $@ `sed -e "/^-/d" -e "s!^!grt/!" < grt/grt-files` \
	 $(GRT_ADD_OBJS) grt/run-bind.o grt/main.o
	$(GRT_RANLIB) $@

grt/run-bind.adb: grt $(GRT_SRC_DEPS) grt-force
	cd grt; $(MSYS2_ARG) $(GNATMAKE) -c -aI$(GRTSRCDIR) -aI.. \
	  $(GRT_PRAGMA_FLAG) ghdl_main $(GRT_ADAFLAGS) -cargs $(GRT_FLAGS)
# 	Set No Run-Time flag to suppress references to standard gnat library.
	sed -e '/^P /s/P /P NR /' < grt/ghdl_main.ali > grt/ghdl_main-tmp.ali
	$(MV) grt/ghdl_main-tmp.ali grt/ghdl_main.ali
	cd grt; $(GNATMAKE) -b ghdl_main.ali -bargs  -Lgrt_ -o run-bind.adb -n

grt:
	mkdir grt

grt/run-bind.o: grt/run-bind.adb
	cd grt; $(MSYS2_ARG) $(GRT_ADACOMPILE) -o run-bind.o run-bind.adb

grt/main.o: $(GRTSRCDIR)/main.adb
	cd grt; $(MSYS2_ARG) $(GRT_ADACOMPILE) -o main.o $< -Igrt

GRT_C_COMPILE = $(CC) -c $(GRT_FLAGS) -o $@ $<
GRT_C_COMPILE_PIC = $(CC) -c $(GRT_FLAGS) $(PIC_FLAGS) -o $@ $<

jumps.o: $(GRTSRCDIR)/config/jumps.c
	$(GRT_C_COMPILE)

pic/jumps.o: $(GRTSRCDIR)/config/jumps.c
	$(GRT_C_COMPILE_PIC)

win32.o: $(GRTSRCDIR)/config/win32.c
	$(GRT_C_COMPILE)

math.o: $(GRTSRCDIR)/config/math.c
	$(GRT_C_COMPILE)

times.o : $(GRTSRCDIR)/config/times.c
	$(GRT_C_COMPILE)

pic/times.o : $(GRTSRCDIR)/config/times.c
	$(GRT_C_COMPILE_PIC)

clock.o : $(GRTSRCDIR)/config/clock.c
	$(GRT_C_COMPILE)

grt-cbinding.o: $(GRTSRCDIR)/grt-cbinding.c
	$(GRT_C_COMPILE)

pic/grt-cbinding.o: $(GRTSRCDIR)/grt-cbinding.c
	$(GRT_C_COMPILE_PIC)

grt-cvpi.o: $(GRTSRCDIR)/grt-cvpi.c
	$(GRT_C_COMPILE)

pic/grt-cvpi.o: $(GRTSRCDIR)/grt-cvpi.c
	$(GRT_C_COMPILE_PIC)

grt-cthreads.o: $(GRTSRCDIR)/grt-cthreads.c
	$(GRT_C_COMPILE)

fstapi.o: $(GRTSRCDIR)/fst/fstapi.c
	$(GRT_C_COMPILE) -I$(GRTSRCDIR)/fst

pic/fstapi.o: $(GRTSRCDIR)/fst/fstapi.c
	$(GRT_C_COMPILE_PIC) -I$(GRTSRCDIR)/fst

lz4.o: $(GRTSRCDIR)/fst/lz4.c
	$(GRT_C_COMPILE)

pic/lz4.o: $(GRTSRCDIR)/fst/lz4.c
	$(GRT_C_COMPILE_PIC)

fastlz.o: $(GRTSRCDIR)/fst/fastlz.c
	$(GRT_C_COMPILE)

pic/fastlz.o: $(GRTSRCDIR)/fst/fastlz.c
	$(GRT_C_COMPILE_PIC)

chkstk.o: $(GRTSRCDIR)/config/chkstk.S
	$(GRT_C_COMPILE)

grt-backtraces-impl.ads:
ifneq ($(GRT_LIBBACKTRACE),)
	echo "with Grt.Backtraces.Gcc;" > $@
	echo "package Grt.Backtraces.Impl renames Grt.Backtraces.Gcc;" >> $@
else
	echo "with Grt.Backtraces.Jit;" > $@
	echo "package Grt.Backtraces.Impl renames Grt.Backtraces.Jit;" >> $@
endif

grt-disp-config:
	@echo "target: $(target)"
	@echo "targ: $(targ)"
	@echo "arch: $(arch)"
	@echo "osys: $(osys)"

grt/grt-files: grt/run-bind.adb
	sed -e "1,/-- *BEGIN/d" -e "/-- *END/,\$$d" \
	  -e "s/   --   //" < $< | tr -d '\r' > $@

# Remove local files (they are now in the libgrt library).
# Also, remove the -shared option, in order not to build a shared library
#  instead of an executable.
# Also remove -lgnat and its associated -L flags.  This appears to be required
#  with GNAT GPL 2005.
grt/grt-files.in: grt/grt-files
	sed -e "\!^.[/\\]!d" -e "/-shared/d" -e "/-static/d" -e "/-lgnat/d" \
	  -e "\X-L/Xd" < $< > $@

$(libdirsuffix)/grt.lst: grt/grt-files.in
	echo "@/libgrt.a" > $@
ifdef GRT_EXTRA_LIB
	for i in $(GRT_EXTRA_LIB); do echo $$i >> $@; done
endif
ifneq ($(LIBBACKTRACE),)
	echo "@/libbacktrace.a" >> $@
endif
	cat $< >> $@

$(libdirsuffix)/grt.ver: $(GRTSRCDIR)/grt.ver
	cp $< $@

ifneq ($(GRT_LIBBACKTRACE),)
$(libdirsuffix)/$(GRT_LIBBACKTRACE): $(LIBBACKTRACE)
	cp $< $@
endif

grt-force:

.PHONY: grt-all grt-force
