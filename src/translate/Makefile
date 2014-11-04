#  -*- Makefile -*- for the GHDL translation back-end.
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

BE=gcc
ortho_srcdir=../ortho
GNAT_FLAGS=-aI.. -aI../psl -gnaty3befhkmr -gnata -gnatf -gnatwael -gnat05
#GNAT_FLAGS+=-O -gnatn
LN=ln -s

compiler: force # ortho_nodes.ads ortho_$(BE)_front.ads
	$(MAKE) -f $(ortho_srcdir)/$(BE)/Makefile \
	 ortho_srcdir=$(ortho_srcdir) GNAT_FLAGS="$(GNAT_FLAGS)" \
	 ortho_exec=ghdl1-$(BE) all

all:
	[ -d lib ] || mkdir lib
	$(MAKE) -f $(ortho_srcdir)/gcc/Makefile \
	 ortho_srcdir=$(ortho_srcdir) GNAT_FLAGS="$(GNAT_FLAGS)" \
	 ortho_exec=ghdl1-gcc all
	$(MAKE) -C ghdldrv
	$(MAKE) -C grt all libdir=`pwd`/lib
	$(MAKE) -C ghdldrv install.v87 install.v93 install.standard

clean:
	$(RM) *.o *.ali ghdl1-* gen_tree ortho_nodes-main b~*.ad?
	$(RM) *~ ortho_nodes.ads ortho_nodes.tmp

force:

.PHONY: compiler clean force all
