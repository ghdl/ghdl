#  Makefile of ortho implementation for GCC.
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
ortho_srcdir=..
orthobe_srcdir=$(ortho_srcdir)/gcc
agcc_srcdir=$(ortho_srcdir)/agcc
agcc_objdir=.
AGCC_GCCSRC_DIR:=$(HOME)/dist/gcc-3.4.3
AGCC_GCCOBJ_DIR:=$(AGCC_GCCSRC_DIR)-objs
SED=sed

all: $(ortho_exec)

include $(agcc_srcdir)/Makefile.inc

ORTHO_BASENAME=$(orthobe_srcdir)/ortho_gcc
ORTHO_PACKAGE=Ortho_Gcc

include $(ortho_srcdir)/Makefile.inc

$(ortho_exec): $(AGCC_DEPS) $(ORTHO_BASENAME).ads force
	gnatmake -m -o $@ -g -aI$(agcc_srcdir) -aI$(ortho_srcdir) \
	 -aI$(ortho_srcdir)/gcc $(GNAT_FLAGS) ortho_gcc-main \
	 -bargs -E -largs $(AGCC_OBJS) #-static

clean: agcc-clean
	$(RM) -f *.o *.ali ortho_nodes-main
	$(RM) b~*.ad? *~

distclean: clean agcc-clean


force:

.PHONY: force all clean

