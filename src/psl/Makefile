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

DEPS=psl-nodes.ads psl-nodes.adb.in $(PNODES)

PNODES_FLAGS=--field-file=psl-nodes.adb.in \
  --kind-file=psl-nodes.ads \
  --node-file=psl-nodes.ads \
  --template-file=psl-nodes.adb.in \
  --meta-basename=psl-nodes_meta \
  --kind-type=Nkind \
  --kind-prefix=N_ \
  --node-type=Node

GEN_FILES=psl-nodes.adb psl-nodes_meta.ads psl-nodes_meta.adb

all: $(GEN_FILES)

psl-nodes.adb: psl-nodes.adb.in $(DEPS)
	$(RM) $@
	$(PNODES) $(PNODES_FLAGS) body > $@
	chmod -w $@

psl-nodes_meta.ads: psl-nodes_meta.ads.in $(DEPS)
	$(RM) $@
	$(PNODES) $(PNODES_FLAGS) meta_specs > $@
	chmod -w $@

psl-nodes_meta.adb: psl-nodes_meta.adb.in $(DEPS)
	$(RM) $@
	$(PNODES) $(PNODES_FLAGS) meta_body > $@
	chmod -w $@

clean:
	$(RM) -f $(GEN_FILES)
