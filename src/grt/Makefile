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
