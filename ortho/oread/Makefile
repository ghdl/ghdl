#  -*- Makefile -*- for the ortho-code compiler.
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
BE = gcc
ortho_srcdir=..
BACK_END=$(ortho_srcdir)/$(BE)
ortho_exec=oread-$(BE)

all: $(ortho_exec)

test: test.s
	$(CC) -o $@ $^

test.s: $(ortho_exec)
	./$(ortho_exec) test

$(ortho_exec): force
	$(MAKE) -f $(BACK_END)/Makefile ortho_exec=$(ortho_exec)

clean:
	$(MAKE) -f $(BACK_END)/Makefile clean
	$(RM) -f oread-gcc oread-mcode *.o *~

distclean: clean
	$(MAKE) -f $(BACK_END)/Makefile distclean

force:

.PHONY: force
