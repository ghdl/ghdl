#  -*- Makefile -*- for the GHDL Run Time library.
#  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <gnu.org/licenses>.

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

# Standard GRT_EXEC_OPTS value for ELF platforms.
GRT_ELF_EXEC_OPTS:=-Wl,--version-script=@/grt.ver -Wl,--export-dynamic

# These conditions define these variables:
#  GRT_TARGET_OBJS: platform specific files to be added in the grt library
#  GRT_EXTRA_LIBS: platform specific libraries and options to be added during
#     link of simulation binaries.  These are library used by grt.
#  GRT_EXEC_OPTS: platform specific options to be added during link of
#     simulation binaries, but not needed when creating a shared library.
#     It is set on ELF platforms to export some symbols (like the vpi
#     functions) so that a vpi object file can refer them.
#  GRT_SHARED_OPTS: platform specific options to be added during link of
#     -shared simulation binaries.  Used instead of GET_EXEC_OPTS
ifeq ($(filter-out mingw32 mingw64 windows,$(osys)),)
  # For windows.
  GRT_TARGET_OBJS=win32.o clock.o
  GRT_EXTRA_LIB=-ldbghelp
  GRT_EXEC_OPTS=
  GRT_SHARED_OPTS=-Wl,-u,_ghdl_main

  ifeq ($(filter-out x86_64,$(arch)),)
    GRT_TARGET_OBJS+=chkstk-x64.o
  else
    GRT_TARGET_OBJS+=chkstk.o
  endif
else
  # For unix variants.
  # Usually needs -dl (for dlopen) and -lm (for math functions).
  # But some platforms provide these features within libc.
  GRT_TARGET_OBJS=jumps.o times.o
  GRT_EXTRA_LIB=
  GRT_EXEC_OPTS=
  GRT_SHARED_OPTS=-Wl,-u,ghdl_main

  ifeq ($(filter-out linux%,$(osys)),)
    GRT_EXTRA_LIB=-ldl -lm
    GRT_EXEC_OPTS=$(GRT_ELF_EXEC_OPTS)
  endif
  ifeq ($(filter-out netbsd freebsd% dragonfly%,$(osys)),)
    GRT_EXTRA_LIB=-lm
    GRT_EXEC_OPTS=$(GRT_ELF_EXEC_OPTS)
  endif
  ifeq ($(filter-out solaris%,$(osys)),)
    GRT_EXTRA_LIB=-ldl -lm
  endif
  ifeq ($(filter-out darwin%,$(osys)),)
    GRT_SHARED_OPTS=-Wl,-u,_ghdl_main
    GRT_EXTRA_LIB=
  endif
endif

# Object files (from C) for fst
GRT_FST_OBJS := fstapi.o lz4.o fastlz.o

# Additionnal object files (C or asm files).
GRT_BASE_C_OBJS:=$(GRT_TARGET_OBJS) grt-cstdio.o grt-cgnatrts.o
GRT_ADD_OBJS:=$(GRT_BASE_C_OBJS) grt-cvpi.o grt-cvhpi.o grt-cdynload.o $(GRT_FST_OBJS)

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

grt-all: $(libghdldirsuffix)/libgrt.a \
  $(libghdldirsuffix)/$(GRT_LIBBACKTRACE) \
  $(libghdldirsuffix)/grt.lst \
  $(libghdldirsuffix)/grt-exec.lst \
  $(libghdldirsuffix)/grt-shared.lst \
  $(libghdldirsuffix)/grt.ver

$(libghdldirsuffix)/libgrt.a: $(GRT_ADD_OBJS) grt/run-bind.o grt/main.o grt/grt-files
	$(RM) -f $@
	$(AR) rcv $@ `sed -e "/^-/d" -e "s!^!grt/!" < grt/grt-files` \
	 $(GRT_ADD_OBJS) grt/run-bind.o grt/main.o
	$(GRT_RANLIB) $@

grt/run-bind.adb: grt $(GRT_SRC_DEPS) grt-force
	cd grt && $(MSYS2_ARG) $(GNATMAKE) -c -aI$(GRTSRCDIR) -aI.. \
	  $(GRT_PRAGMA_FLAG) ghdl_main $(GRT_ADAFLAGS) -cargs $(GRT_FLAGS)
# 	Set No Run-Time flag to suppress references to standard gnat library.
	sed -e '/^P /s/P /P NR /' < grt/ghdl_main.ali > grt/ghdl_main-tmp.ali
	$(MV) grt/ghdl_main-tmp.ali grt/ghdl_main.ali
	cd grt && $(GNATMAKE) -b ghdl_main.ali -bargs  -Lgrt_ -o run-bind.adb -n

grt:
	mkdir grt

grt/run-bind.o: grt grt/run-bind.adb
	cd grt && $(MSYS2_ARG) $(GRT_ADACOMPILE) -o run-bind.o run-bind.adb

grt/main.o: grt $(GRTSRCDIR)/main.adb
	cd grt && $(MSYS2_ARG) $(GRT_ADACOMPILE) -o main.o $(GRTSRCDIR)/main.adb -Igrt

GRT_C_COMPILE = $(CC) -c $(GRT_FLAGS) $(CFLAGS) -o $@ $<
GRT_C_COMPILE_PIC = $(CC) -c $(GRT_FLAGS) $(PIC_FLAGS) $(CFLAGS) -o $@ $<

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

grt-cstdio.o: $(GRTSRCDIR)/grt-cstdio.c
	$(GRT_C_COMPILE)

pic/grt-cstdio.o: $(GRTSRCDIR)/grt-cstdio.c
	$(GRT_C_COMPILE_PIC)

# Note: grt-cgnatrts.o depends on the config.
grt-cgnatrts.o: $(GRTSRCDIR)/grt-cgnatrts.c Makefile
	$(GRT_C_COMPILE)

pic/grt-cgnatrts.o: $(GRTSRCDIR)/grt-cgnatrts.c Makefile
	$(GRT_C_COMPILE_PIC)

grt-cvpi.o: $(GRTSRCDIR)/grt-cvpi.c
	$(GRT_C_COMPILE)

pic/grt-cvpi.o: $(GRTSRCDIR)/grt-cvpi.c
	$(GRT_C_COMPILE_PIC)

grt-cvhpi.o: $(GRTSRCDIR)/grt-cvhpi.c
	$(GRT_C_COMPILE)

pic/grt-cvhpi.o: $(GRTSRCDIR)/grt-cvhpi.c
	$(GRT_C_COMPILE_PIC)

grt-cdynload.o: $(GRTSRCDIR)/grt-cdynload.c
	$(GRT_C_COMPILE)

pic/grt-cdynload.o: $(GRTSRCDIR)/grt-cdynload.c
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

chkstk-x64.o: $(GRTSRCDIR)/config/chkstk-x64.S
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

$(libghdldirsuffix)/grt.lst: grt/grt-files.in
	echo "@/libgrt.a" > $@
	for i in $(GRT_EXTRA_LIB); do echo $$i >> $@; done
ifneq ($(LIBBACKTRACE),)
	echo "@/libbacktrace.a" >> $@
endif
	cat $< >> $@

$(libghdldirsuffix)/grt-exec.lst:
	echo "# link options for executables" > $@
	for i in $(GRT_EXEC_OPTS); do echo $$i >> $@; done

$(libghdldirsuffix)/grt-shared.lst:
	echo "# link options for shared libraries" > $@
	for i in $(GRT_SHARED_OPTS); do echo $$i >> $@; done

$(libghdldirsuffix)/grt.ver: $(GRTSRCDIR)/grt.ver
	cp $< $@

ifneq ($(GRT_LIBBACKTRACE),)
$(libghdldirsuffix)/$(GRT_LIBBACKTRACE): $(LIBBACKTRACE)
	cp $< $@
endif

grt-force:

.PHONY: grt-all grt-force
