PNODES=../../scripts/pnodes.py
PNODESRS=../../scripts/pnodesrs.py
PNODES_ARGS=--field-file=verilog-nodes.adb.in --kind-file=verilog-nodes.ads --node-file=verilog-nodes.ads --template-file=verilog-nodes.adb.in --meta-basename=verilog-nodes_meta --kind-type=Nkind --kind-range-prefix=Nkinds_ --kind-prefix=N_ --node-type=Node

all: verilog-nodes.adb verilog-nodes_meta.ads verilog-nodes_meta.adb \
  nodes_def.rs

verilog-nodes.adb: verilog-nodes.adb.in verilog-nodes.ads $(PNODES)
	$(RM) -f $@
	$(PNODES) $(PNODES_ARGS) body > $@
	chmod a-w $@

verilog-nodes_meta.ads: verilog-nodes_meta.ads.in verilog-nodes.ads $(PNODES)
	$(RM) -f $@
	$(PNODES) $(PNODES_ARGS) meta_specs > $@
	chmod a-w $@

verilog-nodes_meta.adb: verilog-nodes_meta.adb.in verilog-nodes.ads $(PNODES)
	$(RM) -f $@
	$(PNODES) $(PNODES_ARGS) --keep-order meta_body > $@
	chmod a-w $@

nodes_def.rs: $(PNODESRS) verilog-nodes.ads
	$(RM) $@
	$(PNODESRS) $(PNODES_ARGS) verilog-nodes > $@
	chmod -w $@
