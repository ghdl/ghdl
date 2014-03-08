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

$(ortho_srcdir)/$(BE)/$(ORTHO_BASENAME).ads: \
  $(ortho_srcdir)/ortho_nodes.common.ads \
  $(ortho_srcdir)/$(BE)/$(ORTHO_BASENAME).private.ads
	$(RM) -f $@
	echo "--  DO NOT MODIFY - this file was generated from:" > $@
	echo "--  ortho_nodes.common.ads and $(ORTHO_BASENAME).private.ads" \
	  >> $@
	echo "--" >> $@
	$(SED) -e '/^private/,$$d' \
	  < $(ortho_srcdir)/$(BE)/$(ORTHO_BASENAME).private.ads >> $@
	echo "--  Start of common part" >> $@
	$(SED) -e '1,/^package/d' -e '/^private/,$$d' < $< >> $@
	echo "--  End of common part" >> $@
	$(SED) -n -e '/^private/,$$p' \
	  < $(ortho_srcdir)/$(BE)/$(ORTHO_BASENAME).private.ads >> $@
	chmod a-w $@
