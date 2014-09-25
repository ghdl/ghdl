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

DEPS=../iirs.ads ../nodes.ads ./pnodes.py

all: ../iirs.adb ../disp_tree.adb ../nodes_gc.adb

../iirs.adb: ../iirs.adb.in $(DEPS)
	$(RM) $@
	./pnodes.py body > $@
	chmod -w $@

../disp_tree.adb: ../disp_tree.adb.in $(DEPS)
	$(RM) $@
	./pnodes.py disp_tree > $@
	chmod -w $@

../nodes_gc.adb: ../nodes_gc.adb.in $(DEPS)
	$(RM) $@
	./pnodes.py mark_tree > $@
	chmod -w $@
