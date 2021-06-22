#  Copyright (C) 2002 - 2015 Tristan Gingold
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

# Some files are automatically generated using pnodes.py
# This Makefile can be used to regenerate these files.  Generated files must
# be committed and distribued with the sources, so that users don't need to
# regenerate them (and don't need to have python installed).

PNODES=../../scripts/pnodes.py
PNODESPY=../../scripts/pnodespy.py
PYTHON=python3

DEPS=vhdl-nodes.ads vhdl-nodes.adb.in $(PNODES)

GEN_FILES=vhdl-nodes.adb \
  vhdl-nodes_meta.ads \
  vhdl-nodes_meta.adb \
  vhdl-elocations.adb \
  vhdl-elocations_meta.ads \
  vhdl-elocations_meta.adb \
  ../../pyGHDL/libghdl/vhdl/nodes.py \
  ../../pyGHDL/libghdl/vhdl/nodes_meta.py \
  ../../pyGHDL/libghdl/vhdl/tokens.py \
  ../../pyGHDL/libghdl/vhdl/elocations.py \
  ../../pyGHDL/libghdl/errorout.py \
  ../../pyGHDL/libghdl/std_names.py

NODES_FLAGS=--node-file=vhdl-nodes.ads --field-file=vhdl-nodes.adb.in \
  --template-file=vhdl-nodes.adb.in --kind-file=vhdl-nodes.ads \
  --meta-basename=vhdl-nodes_meta

ELOCATIONS_FLAGS=--node-file=vhdl-elocations.ads \
  --field-file=vhdl-elocations.adb.in --kind-file=vhdl-nodes.ads \
  --template-file=vhdl-elocations.adb.in --meta-basename=vhdl-elocations_meta

all: $(GEN_FILES)

vhdl-nodes.adb: vhdl-nodes.adb.in $(DEPS)
	$(RM) $@
	$(PNODES) $(NODES_FLAGS) body > $@
	chmod -w $@

vhdl-nodes_meta.ads: vhdl-nodes_meta.ads.in $(DEPS)
	$(RM) $@
	$(PNODES) $(NODES_FLAGS) meta_specs > $@
	chmod -w $@

vhdl-nodes_meta.adb: vhdl-nodes_meta.adb.in $(DEPS)
	$(RM) $@
	$(PNODES) $(NODES_FLAGS) meta_body > $@
	chmod -w $@

vhdl-elocations.adb: vhdl-elocations.adb.in vhdl-elocations.ads $(DEPS)
	$(RM) $@
	$(PNODES) $(ELOCATIONS_FLAGS) body > $@
	chmod -w $@

vhdl-elocations_meta.ads: vhdl-elocations_meta.ads.in vhdl-elocations.ads $(DEPS)
	$(RM) $@
	$(PNODES) $(ELOCATIONS_FLAGS) meta_specs > $@
	chmod -w $@

vhdl-elocations_meta.adb: vhdl-elocations_meta.adb.in vhdl-elocations.ads $(DEPS)
	$(RM) $@
	$(PNODES) $(ELOCATIONS_FLAGS) meta_body > $@
	chmod -w $@

../../pyGHDL/libghdl/vhdl/nodes.py: $(DEPS) $(PNODESPY)
	$(RM) $@
	$(PNODESPY) $(NODES_FLAGS) libghdl-nodes > $@
	$(PYTHON) -m black $@
	chmod -w $@

../../pyGHDL/libghdl/vhdl/nodes_meta.py: $(DEPS) $(PNODESPY)
	$(RM) $@
	$(PNODESPY) $(NODES_FLAGS) libghdl-meta > $@
	$(PYTHON) -m black $@
	chmod -w $@

../../pyGHDL/libghdl/std_names.py: $(PNODESPY) ../std_names.ads
	$(RM) $@
	$(PNODESPY) $(NODES_FLAGS) libghdl-names > $@
	$(PYTHON) -m black $@
	chmod -w $@

../../pyGHDL/libghdl/vhdl/tokens.py: $(PNODESPY) vhdl-tokens.ads
	$(RM) $@
	$(PNODESPY) $(NODES_FLAGS) libghdl-tokens > $@
	$(PYTHON) -m black $@
	chmod -w $@

../../pyGHDL/libghdl/vhdl/elocations.py: $(PNODESPY) vhdl-elocations.ads
	$(RM) $@
	$(PNODESPY) $(ELOCATIONS_FLAGS) libghdl-elocs > $@
	$(PYTHON) -m black $@
	chmod -w $@

../../pyGHDL/libghdl/errorout.py: $(PNODESPY) ../errorout.ads
	$(RM) $@
	$(PNODESPY) $(ELOCATIONS_FLAGS) libghdl-errorout > $@
	$(PYTHON) -m black $@
	chmod -w $@

clean:
	$(RM) -f $(GEN_FILES)
