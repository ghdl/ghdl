GNATMAKE=gnatmake -j0
CFLAGS=-g
GNAT_FLAGS=-aI.. -aI../vhdl -aI../psl -aI../grt -gnat05 -gnaty3befhkmr -gnata -gnatf -gnatwae $(CFLAGS)
BE=debug
ortho_srcdir=../ortho
GEN_SRCS=edif-nodes.adb edif-nodes_meta.ads edif-nodes_meta.adb
CC=gcc

PNODES=../../scripts/pnodes.py
PNODES_ARGS=--field-file=edif-nodes.adb.in \
  --kind-file=edif-nodes.ads \
  --node-file=edif-nodes.ads \
  --template-file=edif-nodes.adb.in \
  --meta-basename=edif-nodes_meta \
  --kind-type=Nkind \
  --kind-range-prefix=Nkinds_ \
  --kind-prefix=N_ \
  --node-type=Node

all: $(GEN_SRCS)


dump_edif$(EXEEXT): $(GEN_SRCS) force
	$(GNATMAKE) $(GNAT_FLAGS) dump_edif

edif-nodes.adb: edif-nodes.adb.in edif-nodes.ads $(PNODES)
	$(RM) -f $@
	$(PNODES) $(PNODES_ARGS) body > $@
	chmod a-w $@

edif-nodes_meta.ads: edif-nodes_meta.ads.in edif-nodes.ads $(PNODES)
	$(RM) -f $@
	$(PNODES) $(PNODES_ARGS) meta_specs > $@
	chmod a-w $@

edif-nodes_meta.adb: edif-nodes_meta.adb.in edif-nodes.ads $(PNODES)
	$(RM) -f $@
	$(PNODES) $(PNODES_ARGS) --keep-order meta_body > $@
	chmod a-w $@

clean: force
	$(RM) -f *.o *.ali dump_edif  *.cf b~*.ad?

maintainer-clean: clean
	$(RM) -f $(GEN_SRCS)

force:
