#  Copyright (C) 2002 - 2015 Tristan Gingold
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

# Some files are automatically generated using pnodes.py
# This Makefile can be used to regenerate these files.  Generated files must
# be committed and distribued with the sources, so that users don't need to
# regenerate them (and don't need to have python installed).

PNODES=../xtools/pnodes.py
PNODESPY=python/pnodespy.py

DEPS=iirs.ads nodes.ads $(PNODES)

GEN_FILES=iirs.adb nodes_meta.ads nodes_meta.adb \
  elocations.adb elocations_meta.ads elocations_meta.adb \
  python/libghdl/iirs.py python/libghdl/nodes_meta.py \
  python/libghdl/std_names.py python/libghdl/tokens.py \
  python/libghdl/elocations.py

ELOCATIONS_FLAGS=--node-file=elocations.ads --field-file=elocations.adb.in \
  --template-file=elocations.adb.in --meta-basename=elocations_meta

all: $(GEN_FILES)

iirs.adb: iirs.adb.in $(DEPS)
	$(RM) $@
	$(PNODES) body > $@
	chmod -w $@

nodes_meta.ads: nodes_meta.ads.in $(DEPS)
	$(RM) $@
	$(PNODES) meta_specs > $@
	chmod -w $@

nodes_meta.adb: nodes_meta.adb.in $(DEPS)
	$(RM) $@
	$(PNODES) meta_body > $@
	chmod -w $@

elocations.adb: elocations.adb.in elocations.ads $(DEPS)
	$(RM) $@
	$(PNODES) $(ELOCATIONS_FLAGS) body > $@
	chmod -w $@

elocations_meta.ads: elocations_meta.ads.in elocations.ads $(DEPS)
	$(RM) $@
	$(PNODES) $(ELOCATIONS_FLAGS) meta_specs > $@
	chmod -w $@

elocations_meta.adb: elocations_meta.adb.in elocations.ads $(DEPS)
	$(RM) $@
	$(PNODES) $(ELOCATIONS_FLAGS) meta_body > $@
	chmod -w $@

python/libghdl/iirs.py: $(DEPS) $(PNODESPY)
	$(RM) $@
	$(PNODESPY) libghdl-iirs > $@
	chmod -w $@

python/libghdl/nodes_meta.py: $(DEPS) $(PNODESPY)
	$(RM) $@
	$(PNODESPY) libghdl-meta > $@
	chmod -w $@

python/libghdl/std_names.py: $(PNODESPY) ../std_names.ads
	$(RM) $@
	$(PNODESPY) libghdl-names > $@
	chmod -w $@

python/libghdl/tokens.py: $(PNODESPY) tokens.ads
	$(RM) $@
	$(PNODESPY) libghdl-tokens > $@
	chmod -w $@

python/libghdl/elocations.py: $(PNODESPY) elocations.ads
	$(RM) $@
	$(PNODESPY) $(ELOCATIONS_FLAGS) libghdl-elocs > $@
	chmod -w $@

clean:
	$(RM) -f $(GEN_FILES)
