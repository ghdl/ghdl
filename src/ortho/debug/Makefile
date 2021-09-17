#  -*- Makefile -*- for the ortho-code back-end
#  Copyright (C) 2005 Tristan Gingold
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
BE=debug
ortho_srcdir=..

orthobe_srcdir=$(ortho_srcdir)/$(BE)

GNATMAKE=gnatmake
CC=gcc
CFLAGS=-g
ALL_GNAT_FLAGS=-pipe -g -gnato -gnatwaeu -gnatf -gnaty3befhkmr
GNATMAKE_FLAGS=$(ALL_GNAT_FLAGS) $(GNATFLAGS) -aI$(ortho_srcdir) -aI$(orthobe_srcdir) -aI.
SED=sed

all: $(ortho_exec)


$(ortho_exec): force $(ortho_srcdir)/$(BE)/ortho_debug.ads
	$(GNATMAKE) -o $@ $(GNATMAKE_FLAGS) ortho_debug-main \
	 -bargs -E -largs $(LDFLAGS) $(GNATLFLAGS)

clean:
	$(RM) -f *.o *.ali *~ b~*.ad? ortho_nodes-main
	$(RM) ortho_debug.ads

force:

ORTHO_BASENAME=ortho_debug

# Automatically build ortho_debug.ads from ortho_node.common.ads and
# ortho_debug.private.ads
include $(ortho_srcdir)/Makefile.inc
