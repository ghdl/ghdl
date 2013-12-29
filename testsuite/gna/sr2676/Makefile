# project name
PROJECT=reset
# vhdl files
FILES = reset.vhdl reset_types.vhdl reset-rtl.vhdl
# testbench
SIMTOP = reset_testbench
SIMFILES = reset-test.vhdl
# Simu break condition
GHDL_SIM_OPT    = --assert-level=error --ieee-asserts=disable
#GHDL_SIM_OPT    = --stop-time=1ms
SIMDIR = sim

GHDL_CMD        = ghdl
GHDL_FLAGS      = --warn-no-vital-generic 
VIEW_CMD        = /usr/bin/gtkwave

ghdl-compile:
	mkdir -p $(SIMDIR)
	$(GHDL_CMD) -i $(GHDL_FLAGS) --workdir=$(SIMDIR) --work=work $(SIMFILES) $(FILES)
	$(GHDL_CMD) -m $(GHDL_FLAGS) --workdir=$(SIMDIR) --work=work $(SIMTOP)
	@mv $(SIMTOP) $(SIMDIR)/$(SIMTOP)

ghdl-run: ghdl-compile
	@$(SIMDIR)/$(SIMTOP) $(GHDL_SIM_OPT) --vcdgz=$(SIMDIR)/$(SIMTOP).vcdgz

ghdl-view: 
	gunzip --stdout $(SIMDIR)/$(SIMTOP).vcdgz | $(VIEW_CMD) --vcd &

ghdl-clean :


