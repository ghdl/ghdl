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
# ADAC: the GNAT compiler
# GHDL1: the ghdl compiler
# GRT_RANLIB: the ranlib tool for the grt library.
# grt_libdir: the place to put grt.
# GRTSRCDIR: the source directory of grt.
# target: GCC target
# GRT_FLAGS: common (Ada + C + asm) compilation flags.
# GRT_ADAFLAGS: compilation flags for Ada

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
ifeq ($(filter-out mingw32,$(osys)),)
  GRT_TARGET_OBJS=jumps.o math.o clock.o
else
  GRT_TARGET_OBJS=jumps.o times.o
  ifeq ($(filter-out linux,$(osys)),)
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
GRT_ADD_OBJS:=$(GRT_TARGET_OBJS) grt-cbinding.o grt-cvpi.o $(GRT_FST_OBJS)

#GRT_USE_PTHREADS=y
ifeq ($(GRT_USE_PTHREADS),y)
 GRT_CFLAGS+=-DUSE_THREADS
 GRT_ADD_OBJS+=grt-cthreads.o
 GRT_EXTRA_LIB+=-lpthread
endif

# Configuration pragmas.
GRT_PRAGMA_FLAG=-gnatec$(GRTSRCDIR)/grt.adc -gnat05

# Rule to compile an Ada file.
GRT_ADACOMPILE=$(ADAC) -c $(GRT_FLAGS) $(GRT_PRAGMA_FLAG) -o $@ $<

grt-all: libgrt.a grt.lst

libgrt.a: $(GRT_ADD_OBJS) run-bind.o main.o grt-files
	$(RM) -f $@
	$(AR) rcv $@ `sed -e "/^-/d" < grt-files` $(GRT_ADD_OBJS) \
	 run-bind.o main.o
	$(GRT_RANLIB) $@

run-bind.adb: grt-force
	gnatmake -c -aI$(GRTSRCDIR) $(GRT_PRAGMA_FLAG) \
	  ghdl_main $(GRT_ADAFLAGS) -cargs $(GRT_FLAGS)
	gnatbind -Lgrt_ -o run-bind.adb -n ghdl_main.ali

#system.ads:
#	sed -e "/Configurable_Run_Time/s/False/True/" \
#	 -e "/Suppress_Standard_Library/s/False/True/" \
#	  < `$(ADAC) -print-file-name=adainclude/system.ads` > $@

run-bind.o: run-bind.adb
	$(GRT_ADACOMPILE)

main.o: $(GRTSRCDIR)/main.adb
	$(GRT_ADACOMPILE)

jumps.o: $(GRTSRCDIR)/config/jumps.c
	$(CC) -c $(GRT_FLAGS) $(GRT_CFLAGS) -o $@ $<

win32.o: $(GRTSRCDIR)/config/win32.c
	$(CC) -c $(GRT_FLAGS) -o $@ $<

math.o: $(GRTSRCDIR)/config/math.c
	$(CC) -c $(GRT_FLAGS) -o $@ $<

times.o : $(GRTSRCDIR)/config/times.c
	$(CC) -c $(GRT_FLAGS) -o $@ $<

clock.o : $(GRTSRCDIR)/config/clock.c
	$(CC) -c $(GRT_FLAGS) -o $@ $<

grt-cbinding.o: $(GRTSRCDIR)/grt-cbinding.c
	$(CC) -c $(GRT_FLAGS) -o $@ $<

grt-cvpi.o: $(GRTSRCDIR)/grt-cvpi.c
	$(CC) -c $(GRT_FLAGS) -o $@ $<

grt-cthreads.o: $(GRTSRCDIR)/grt-cthreads.c
	$(CC) -c $(GRT_FLAGS) -o $@ $<

fstapi.o: $(GRTSRCDIR)/fst/fstapi.c
	$(CC) -c $(GRT_FLAGS) -o $@ -I$(GRTSRCDIR)/fst $<

lz4.o: $(GRTSRCDIR)/fst/lz4.c
	$(CC) -c $(GRT_FLAGS) -o $@ $<

fastlz.o: $(GRTSRCDIR)/fst/fastlz.c
	$(CC) -c $(GRT_FLAGS) -o $@ $<

chkstk.o: $(GRTSRCDIR)/config/chkstk.S
	$(CC) -c $(GRT_FLAGS) -o $@ $<

grt-disp-config:
	@echo "target: $(target)"
	@echo "targ: $(targ)"
	@echo "arch: $(arch)"
	@echo "osys: $(osys)"

grt-files: run-bind.adb
	sed -e "1,/-- *BEGIN/d" -e "/-- *END/,\$$d" \
	  -e "s/   --   //" < $< > $@

# Remove local files (they are now in the libgrt library).
# Also, remove the -shared option, in order not to build a shared library
#  instead of an executable.
# Also remove -lgnat and its associated -L flags.  This appears to be required
#  with GNAT GPL 2005.
grt-files.in: grt-files
	sed -e "\!^./!d" -e "/-shared/d" -e "/-static/d" -e "/-lgnat/d" \
	  -e "\X-L/Xd" < $< > $@

grt.lst: grt-files.in
	echo "@/libgrt.a" > $@
ifdef GRT_EXTRA_LIB
	for i in $(GRT_EXTRA_LIB); do echo $$i >> $@; done
endif
	cat $< >> $@

grt-install: libgrt.a grt.lst
	$(INSTALL_DATA) libgrt.a $(DESTDIR)$(grt_libdir)/libgrt.a
	$(INSTALL_DATA) grt.lst $(DESTDIR)$(grt_libdir)/grt.lst

grt-force:

grt-clean: grt-force
	$(RM) *.o *.ali run-bind.adb run-bind.ads *.a std_standard.s
	$(RM) grt-files grt-files.in grt.lst

.PHONY: grt-all grt-force grt-clean grt-install
