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
# GRT_FLAGS: compilation flags.

# Set target files.
ifeq ($(filter-out i%86-pc-linux-gnu,$(target)),)
  GRT_TARGET_OBJS=i386.o linux.o times.o
  GRT_EXTRA_LIB=-ldl
endif
ifeq ($(filter-out sparc-sun-solaris%,$(target)),)
  GRT_TARGET_OBJS=sparc.o linux.o times.o
  GRT_EXTRA_LIB=-ldl
endif
ifeq ($(filter-out powerpc-linux%,$(target)),)
  GRT_TARGET_OBJS=ppc.o linux.o times.o
  GRT_EXTRA_LIB=-ldl
endif
ifeq ($(filter-out i%86-pc-mingw32,$(target)),)
  GRT_TARGET_OBJS=win32.o clock.o
endif
ifeq ($(filter-out i%86-pc-cygwin,$(target)),)
  GRT_TARGET_OBJS=win32.o clock.o
endif
# Fall-back: use a generic implementation based on pthreads.
ifndef GRT_TARGET_OBJS
  GRT_TARGET_OBJS=pthread.o times.o
  GRT_EXTRA_LIB=-lpthread
endif

GRT_ADD_OBJS=$(GRT_TARGET_OBJS) grt-cbinding.o grt-cvpi.o

GRT_PRAGMA_FLAG=-gnatec$(GRTSRCDIR)/grt.adc

GRT_OBJS_FILES=\
grt.o \
grt-disp.o \
grt-main.o \
grt-stacks.o \
grt-errors.o \
grt-options.o \
grt-stdio.o \
grt-files.o \
grt-processes.o \
grt-typedesc.o \
grt-hierarchy.o \
grt-shadow_ieee.o \
grt-types.o \
grt-images.o \
grt-signals.o \
grt-vcd.o \
grt-vpi.o \
grt-lib.o \
grt-sdf.o \
grt-stack2.o \
grt-names.o

GRT_ADACOMPILE=$(ADAC) -c $(GRT_FLAGS) -o $@ $<

grt-all: libgrt.a grt.lst

libgrt.a: $(GRT_ADD_OBJS) run-bind.o main.o grt-files
	$(RM) -f $@
	$(AR) rcv $@ `sed -e "/^-/d" < grt-files` $(GRT_ADD_OBJS) \
	 run-bind.o main.o
	$(GRT_RANLIB) $@

run-bind.adb: grt-force
	gnatmake -c $(GNATFLAGS) -aI$(GRTSRCDIR) $(GRT_PRAGMA_FLAG) ghdl_main \
	  -cargs $(GRT_FLAGS)
	gnatbind -Lgrt_ -o run-bind.adb -n ghdl_main.ali

run-bind.o: run-bind.adb
	$(GRT_ADACOMPILE)

main.o: $(GRTSRCDIR)/main.adb
	$(GRT_ADACOMPILE)

i386.o: $(GRTSRCDIR)/config/i386.S
	$(CC) -c $(GRT_FLAGS) -o $@ $<

sparc.o: $(GRTSRCDIR)/config/sparc.S
	$(CC) -c $(GRT_FLAGS) -o $@ $<

ppc.o: $(GRTSRCDIR)/config/ppc.S
	$(CC) -c $(GRT_FLAGS) -o $@ $<

linux.o: $(GRTSRCDIR)/config/linux.c
	$(CC) -c $(GRT_FLAGS) -o $@ $<

win32.o: $(GRTSRCDIR)/config/win32.c
	$(CC) -c $(GRT_FLAGS) -o $@ $<

pthread.o: $(GRTSRCDIR)/config/pthread.c
	$(CC) -c $(GRT_FLAGS) -o $@ $<

times.o : $(GRTSRCDIR)/config/times.c
	$(CC) -c $(GRT_FLAGS) -o $@ $<

clock.o : $(GRTSRCDIR)/config/clock.c
	$(CC) -c $(GRT_FLAGS) -o $@ $<

grt-cbinding.o: $(GRTSRCDIR)/grt-cbinding.c
	$(CC) -c $(GRT_FLAGS) -o $@ $<

grt-cvpi.o: $(GRTSRCDIR)/grt-cvpi.c
	$(CC) -c $(GRT_FLAGS) -o $@ $<

grt-files: run-bind.adb
	sed -e "1,/-- *BEGIN/d" -e "/-- *END/,\$$d" \
	  -e "s/   --   //" < $< > $@

# Remove local files (they are now in the libgrt library).
# Also, remove the -shared option, in order not to build a shared library
# instead of an executable.
grt-files.in: grt-files
	sed -e "\!^./!d" -e "/-shared/d" < $< > $@

grt.lst: grt-files.in
	echo "@/libgrt.a" > $@
ifdef GRT_EXTRA_LIB
	echo $(GRT_EXTRA_LIB) >> $@
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
