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

DEPS=../vhdl/iirs.ads ../vhdl/nodes.ads ./pnodes.py

all: ../vhdl/iirs.adb ../vhdl/nodes_meta.ads ../vhdl/nodes_meta.adb

../vhdl/iirs.adb: ../vhdl/iirs.adb.in $(DEPS)
	$(RM) $@
	./pnodes.py body > $@
	chmod -w $@

../vhdl/nodes_meta.ads: ../vhdl/nodes_meta.ads.in $(DEPS)
	$(RM) $@
	./pnodes.py meta_specs > $@
	chmod -w $@

../vhdl/nodes_meta.adb: ../vhdl/nodes_meta.adb.in $(DEPS)
	$(RM) $@
	./pnodes.py meta_body > $@
	chmod -w $@
