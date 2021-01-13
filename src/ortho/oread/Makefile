#  -*- Makefile -*- for the ortho-code compiler.
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
