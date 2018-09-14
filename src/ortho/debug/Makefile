#  -*- Makefile -*- for the ortho-code back-end
#  Copyright (C) 2005 Tristan Gingold
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
BE=debug
ortho_srcdir=..

orthobe_srcdir=$(ortho_srcdir)/$(BE)

GNATMAKE=gnatmake
CC=gcc
CFLAGS=-g
ALL_GNAT_FLAGS=-pipe -g -gnato -gnatwaeu -gnatf -gnaty3befhkmr
GNATMAKE_FLAGS=-m $(ALL_GNAT_FLAGS) $(GNATFLAGS) -aI$(ortho_srcdir) -aI$(orthobe_srcdir) -aI.
#LARGS=-largs -static
SED=sed

all: $(ortho_exec)


$(ortho_exec): force $(ortho_srcdir)/$(BE)/ortho_debug.ads
	$(GNATMAKE) -o $@ $(GNATMAKE_FLAGS) ortho_debug-main -bargs -E $(LARGS)

clean:
	$(RM) -f *.o *.ali *~ b~*.ad? ortho_nodes-main
	$(RM) ortho_debug.ads

force:

ORTHO_BASENAME=ortho_debug

# Automatically build ortho_debug.ads from ortho_node.common.ads and
# ortho_debug.private.ads
include $(ortho_srcdir)/Makefile.inc
