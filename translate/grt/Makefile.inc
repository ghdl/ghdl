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

# Convert the target variable into a space separated list of architecture,
# manufacturer, and operating system and assign each of those to its own
# variable.

targ:=$(subst -, ,$(target))
arch:=$(word 1,$(targ))
ifeq ($(words $(targ)),2)
  osys:=$(word 2,$(targ))
else
  osys:=$(word 3,$(targ))
endif

# Set target files.
ifeq ($(filter-out i%86 linux,$(arch) $(osys)),)
  GRT_TARGET_OBJS=i386.o linux.o times.o
  GRT_EXTRA_LIB=-ldl
endif
ifeq ($(filter-out x86_64 linux,$(arch) $(osys)),)
  GRT_TARGET_OBJS=amd64.o linux.o times.o
  GRT_EXTRA_LIB=-ldl
endif
ifeq ($(filter-out sparc solaris%,$(arch) $(osys)),)
  GRT_TARGET_OBJS=sparc.o linux.o times.o
  GRT_EXTRA_LIB=-ldl
endif
ifeq ($(filter-out powerpc linux%,$(arch) $(osys)),)
  GRT_TARGET_OBJS=ppc.o linux.o times.o
  GRT_EXTRA_LIB=-ldl
endif
ifeq ($(filter-out ia64 linux,$(arch) $(osys)),)
  GRT_TARGET_OBJS=ia64.o linux.o times.o
  GRT_EXTRA_LIB=-ldl
endif
ifeq ($(filter-out i%86 mingw32,$(arch) $(osys)),)
  GRT_TARGET_OBJS=win32.o clock.o
endif
ifeq ($(filter-out i%86 cygwin,$(arch) $(osys)),)
  GRT_TARGET_OBJS=win32.o clock.o
endif
# Fall-back: use a generic implementation based on pthreads.
ifndef GRT_TARGET_OBJS
  GRT_TARGET_OBJS=pthread.o times.o
  GRT_EXTRA_LIB=-lpthread -ldl
endif

# Additionnal object files (C or asm files).
GRT_ADD_OBJS=$(GRT_TARGET_OBJS) grt-cbinding.o grt-cvpi.o

# Configuration pragmas.
GRT_PRAGMA_FLAG=-gnatec$(GRTSRCDIR)/grt.adc

# Rule to compile an Ada file.
GRT_ADACOMPILE=$(ADAC) -c $(GRT_FLAGS) $(GRT_PRAGMA_FLAG) -o $@ $<

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

ia64.o: $(GRTSRCDIR)/config/ia64.S
	$(CC) -c $(GRT_FLAGS) -o $@ $<

amd64.o: $(GRTSRCDIR)/config/amd64.S
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
