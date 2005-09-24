#  Common -*- Makefile -*- for ortho implementations.
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

# Variable to be defined:
#   SED: sed the stream editor
#   ORTHO_BASENAME

$(ORTHO_BASENAME).ads: $(ortho_srcdir)/ortho_nodes.common.ads \
  $(ORTHO_BASENAME).private.ads
	$(SED) -e '/^package/,$$d' \
	  < $(ORTHO_BASENAME).private.ads \
	  > tmp.prv.hdr
	$(SED) -e '1,/^private/d' -e '/^end/d' \
	  < $(ORTHO_BASENAME).private.ads \
	  > tmp.prv.dcl
	$(SED) -e '1,/^package/d' -e '/^private/,$$d' \
	  < $(ORTHO_BASENAME).private.ads \
	  > tmp.prv.pub
	$(SED) \
	  -e '/^   --- PRIVATE/r tmp.prv.dcl' \
	  -e '/^--- PRIVATE CONTEXT CLAUSES/r tmp.prv.hdr' \
	  -e '/^   --- PUBLIC DECLARATIONS/r tmp.prv.pub' \
	  -e '/--- PRIVATE/d' \
	  -e 's/ORTHO_NODES/$(ORTHO_PACKAGE)/g' < $< > $@
	$(RM) -f tmp.prv.dcl tmp.prv.hdr tmp.prv.pub

