#  -*- Makefile -*- for the gcc implemantation of ortho.
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

# Variable used:
# AGCC_GCCSRC_DIR: the gcc source base directory (ie gcc-X.Y.Z-objs/)
# AGCC_GCCOBJ_DIR: the gcc objects base directory
# agcc_srcdir: the agcc source directory
# agcc_objdir: the agcc object directory

AGCC_INC_FLAGS=-I$(AGCC_GCCOBJ_DIR)/gcc -I$(AGCC_GCCSRC_DIR)/include \
 -I$(AGCC_GCCSRC_DIR)/gcc -I$(AGCC_GCCSRC_DIR)/gcc/config \
 -I$(AGCC_GCCSRC_DIR)/libcpp/include
AGCC_CFLAGS=-g -DIN_GCC $(AGCC_INC_FLAGS)

AGCC_LOCAL_OBJS=ortho-lang.o gcc-version.o

AGCC_DEPS := $(AGCC_LOCAL_OBJS)
AGCC_OBJS := $(AGCC_LOCAL_OBJS) \
	$(AGCC_GCCOBJ_DIR)/gcc/toplev.o \
	$(AGCC_GCCOBJ_DIR)/gcc/c-convert.o \
	$(AGCC_GCCOBJ_DIR)/gcc/libbackend.a \
	$(AGCC_GCCOBJ_DIR)/libcpp/libcpp.a \
	$(AGCC_GCCOBJ_DIR)/libiberty/libiberty.a

gcc-version.c: $(AGCC_GCCSRC_DIR)/gcc/version.c
	-$(RM) -f $@
	echo '#include "version.h"' > $@
	sed  -n -e '/version_string/ s/";/ (ghdl)";/p' < $< >> $@
	echo 'const char bug_report_url[] = "<URL:http://gna.org/projects/ghdl>";' >> $@

gcc-version.o: gcc-version.c
	$(CC) -c -o $@ $< $(AGCC_CFLAGS)

ortho-lang.o: $(agcc_srcdir)/ortho-lang.c \
 $(AGCC_GCCOBJ_DIR)/gcc/gtype-vhdl.h \
 $(AGCC_GCCOBJ_DIR)/gcc/gt-vhdl-ortho-lang.h
	$(CC) -c -o $@ $< $(AGCC_CFLAGS)

agcc-clean: force
	$(RM) -f $(agcc_objdir)/*.o
	$(RM) -f $(agcc_srcdir)/*~

agcc-maintainer-clean: force
	$(RM) -f $(AGCC_DEPS)


.PHONY: agcc-clean agcc-maintainer-clean
