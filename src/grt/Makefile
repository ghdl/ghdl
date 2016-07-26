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
GRT_FLAGS=-g -O
GRT_ADAFLAGS=-gnatn

ADAC=gcc
CC=gcc
GNATFLAGS=$(CFLAGS) -gnatf -gnaty3befhkmr -gnatwlu
GHDL1=../ghdl1-gcc$(EXEEXT)
GRTSRCDIR=.
GRT_RANLIB=ranlib

INSTALL=install
INSTALL_DATA=$(INSTALL) -m 644

prefix=/usr/local
exec_prefix=$(prefix)
libdir=$(exec_prefix)/lib
grt_libdir=$(libdir)

target:=$(shell $(CC) -dumpmachine)

all: grt-all
install: grt-install
clean: grt-clean
	$(RM) *~

show_target:
	echo "Target is $(target)"

include Makefile.inc


GRT_CFLAGS=$(GRT_FLAGS) -Wall
ghwdump: ghwdump.o ghwlib.o
	$(CC) $(GRT_CFLAGS) -o $@ ghwdump.o ghwlib.o

ghwlib.o: ghwlib.c ghwlib.h
	$(CC) -c $(GRT_CFLAGS) -o $@ $<
ghwdump.o: ghwdump.c ghwlib.h
	$(CC) -c $(GRT_CFLAGS) -o $@ $<
