SRC += ../src/Example_Design/duplicator.vhd
SRC += ../src/Example_Design/hexifier.vhd
SRC += ../src/Example_Design/serializer.vhd
SRC += ../src/Example_Design/merginator.vhd
SRC += ../src/SDCard/sdcard_globals.vhd
SRC += ../src/SDCard/sdcard_cmd.vhd
SRC += ../src/Example_Design/sdcard_cmd_logger.vhd
SRC += ../src/SDCard/sdcard_dat.vhd
SRC += ../src/SDCard/sdcard_ctrl.vhd
SRC += ../src/SDCard/sdcard_wrapper.vhd
SRC += sdcard_sim.vhd
#SRC += tb_cmd.vhd
#SRC += tb_sdcard.vhd


#DUT ?= cmd
DUT ?= sdcard


TB = tb_$(DUT)
SRC += $(TB).vhd
WAVE = $(TB).ghw
SAVE = $(TB).gtkw

sim: $(SRC)
	ghdl -c --std=08 $(SRC) -r $(TB) $(GENERIC) --assert-level=error --wave=$(WAVE) --stop-time=200us

questa: $(SRC)
	vcom -2008 $(SRC)
	vsim $(TB)

show: $(WAVE)
	gtkwave $(WAVE) $(SAVE)

formal: $(DUT)_bmc/PASS
$(DUT)_bmc/PASS: $(DUT).sby $(DUT).psl $(DUT).vhd
	sby --yosys "yosys -m ghdl" -f $(DUT).sby

show_bmc:
	gtkwave $(DUT)_bmc/engine_0/trace.vcd $(DUT).gtkw

show_cover:
	gtkwave $(DUT)_cover/engine_0/trace.vcd $(DUT).gtkw

clean:
	rm -rf *.o
	rm -rf work-obj08.cf
	rm -rf $(TB)
	rm -rf $(WAVE)
	rm -rf $(DUT)_bmc
	rm -rf $(DUT)_cover

