#  -*- Makefile -*- for agcc, the Ada binding for GCC internals.
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

# Variable used:
# AGCC_GCCSRC_DIR: the gcc source base directory (ie gcc-X.Y.Z-objs/)
# AGCC_GCCOBJ_DIR: the gcc objects base directory
# agcc_srcdir: the agcc source directory
# agcc_objdir: the agcc object directory

AGCC_INC_FLAGS=-I$(AGCC_GCCOBJ_DIR)/gcc -I$(AGCC_GCCSRC_DIR)/include \
 -I$(AGCC_GCCSRC_DIR)/gcc -I$(AGCC_GCCSRC_DIR)/gcc/config
AGCC_CFLAGS=-g -DIN_GCC $(AGCC_INC_FLAGS)

AGCC_LOCAL_OBJS=agcc-bindings.o agcc-version.o

AGCC_DEPS := $(agcc_srcdir)/agcc-trees.ads \
  $(agcc_srcdir)/agcc-hwint.ads \
  $(agcc_srcdir)/agcc-hconfig.ads \
  $(agcc_srcdir)/agcc-real.ads \
  $(agcc_srcdir)/agcc-machmode.ads \
  $(agcc_srcdir)/agcc-tm.ads \
  $(agcc_srcdir)/agcc-options.ads \
  $(AGCC_LOCAL_OBJS)
AGCC_OBJS := $(AGCC_LOCAL_OBJS) \
	$(AGCC_GCCOBJ_DIR)/gcc/toplev.o \
	$(AGCC_GCCOBJ_DIR)/gcc/c-convert.o \
	$(AGCC_GCCOBJ_DIR)/gcc/libbackend.a \
	$(AGCC_GCCOBJ_DIR)/libiberty/libiberty.a

# Set rights to prevent editing.
GENERATE_VIA_GEN_TREE=\
  $(RM) -f $@ && \
  $(agcc_objdir)/gen_tree -C $(AGCC_GCCOBJ_DIR)/gcc - < $< > $@ && \
  chmod a-w $@

$(agcc_srcdir)/agcc-trees.ads: $(agcc_srcdir)/agcc-trees.ads.in \
   $(agcc_objdir)/gen_tree
	$(GENERATE_VIA_GEN_TREE)

$(agcc_srcdir)/agcc-hwint.ads: $(agcc_srcdir)/agcc-hwint.ads.in \
   $(agcc_objdir)/gen_tree
	$(GENERATE_VIA_GEN_TREE)

$(agcc_srcdir)/agcc-hconfig.ads: $(agcc_srcdir)/agcc-hconfig.ads.in \
   $(agcc_objdir)/gen_tree
	$(GENERATE_VIA_GEN_TREE)

$(agcc_srcdir)/agcc-real.ads: $(agcc_srcdir)/agcc-real.ads.in \
   $(agcc_objdir)/gen_tree
	$(GENERATE_VIA_GEN_TREE)

$(agcc_srcdir)/agcc-machmode.ads: $(agcc_srcdir)/agcc-machmode.ads.in \
   $(agcc_objdir)/gen_tree \
   $(AGCC_GCCOBJ_DIR)/gcc/insn-modes.h
	$(GENERATE_VIA_GEN_TREE)

$(agcc_srcdir)/agcc-tm.ads: $(agcc_srcdir)/agcc-tm.ads.in \
 $(agcc_objdir)/gen_tree
	$(GENERATE_VIA_GEN_TREE)

$(agcc_srcdir)/agcc-options.ads: $(agcc_srcdir)/agcc-options.ads.in \
 $(agcc_objdir)/gen_tree \
 $(AGCC_GCCOBJ_DIR)/gcc/options.h
	$(GENERATE_VIA_GEN_TREE)

$(agcc_objdir)/gen_tree: $(agcc_objdir)/gen_tree.o
	$(CC) -o $@ $<

$(agcc_objdir)/gen_tree.o: $(agcc_srcdir)/gen_tree.c \
  $(AGCC_GCCSRC_DIR)/gcc/tree.def $(AGCC_GCCSRC_DIR)/gcc/tree.h \
  $(AGCC_GCCOBJ_DIR)/gcc/tree-check.h
	$(CC) -c -o $@ $< $(AGCC_CFLAGS)

agcc-bindings.o: $(agcc_srcdir)/agcc-bindings.c \
 $(AGCC_GCCOBJ_DIR)/gcc/gtype-vhdl.h \
 $(AGCC_GCCOBJ_DIR)/gcc/gt-vhdl-agcc-bindings.h
	$(CC) -c -o $@ $< $(AGCC_CFLAGS)

agcc-version.c: $(AGCC_GCCSRC_DIR)/gcc/version.c
	-$(RM) -f $@
	echo '#include "version.h"' > $@
	sed  -n -e '/version_string/ s/";/ (ghdl)";/p' < $< >> $@
	echo 'const char bug_report_url[] = "<URL:mailto:ghdl@free.fr>";' >> $@

agcc-version.o: agcc-version.c
	$(CC) -c -o $@ $< $(AGCC_CFLAGS)

agcc-clean: force
	$(RM) -f $(agcc_objdir)/gen_tree $(agcc_objdir)/gen_tree.o
	$(RM) -f $(agcc_objdir)/*.o
	$(RM) -f $(agcc_srcdir)/*~

agcc-maintainer-clean: force
	$(RM) -f $(AGCC_DEPS)


.PHONY: agcc-clean agcc-maintainer-clean
