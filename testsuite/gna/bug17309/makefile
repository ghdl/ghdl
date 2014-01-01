# Builds the library file "polyamplib", and a lot of testbenches.

.SUFFIXES :
.SUFFIXES : .o .vhdl

AN_OPTS = --workdir=work
ANTB_OPTS = --workdir=work --ieee=synopsys
EL_OPTS = --workdir=work --ieee=synopsys

OPTIONS=--ieee=synopsys

#TB3REQ = command_decoder.o 

# General rule that don't recognize the working directory
#.vhdl.o : 
#       ghdl -a ${AN_OPTS} $<


# Add testbenches to run here
tests: prio_resolv2_tb.vcd active_input_tb.vcd one_of_n_encoder_tb.vcd master_state_machine_tb.vcd

.PHONY: tests

.PHONY: clean
clean:
	ghdl --clean ${AN_OPTS}

work/polyamplib.o : polyamplib.vhdl
	ghdl -a ${AN_OPTS} polyamplib.vhdl

ad_interface_tb.vcd: ad_interface_tb.vhdl work/polyamplib.o
	ghdl -a ${ANTB_OPTS} ad_interface_tb.vhdl
	ghdl -e ${EL_OPTS} ad_interface_tb
	ghdl -r ${EL_OPTS} ad_interface_tb --vcd=ad_interface_tb.vcd

prio_resolv2_tb.vcd: prio_resolv2_tb.vhdl work/polyamplib.o
	ghdl -a ${ANTB_OPTS} prio_resolv2_tb.vhdl
	ghdl -e ${EL_OPTS} prio_resolv2_tb
	ghdl -r ${EL_OPTS} prio_resolv2_tb --vcd=prio_resolv2_tb.vcd

active_input_tb.vcd: active_input_tb.vhdl work/polyamplib.o
	ghdl -a ${ANTB_OPTS} active_input_tb.vhdl
	ghdl -e ${EL_OPTS} active_input_tb
	ghdl -r ${EL_OPTS} active_input_tb --vcd=active_input_tb.vcd

one_of_n_encoder_tb.vcd: one_of_n_encoder_tb.vhdl work/polyamplib.o
	ghdl -a ${ANTB_OPTS} one_of_n_encoder_tb.vhdl
	ghdl -e ${EL_OPTS} one_of_n_encoder_tb
	ghdl -r ${EL_OPTS} one_of_n_encoder_tb --vcd=one_of_n_encoder_tb.vcd

master_state_machine_tb.vcd: master_state_machine_tb.vhdl work/polyamplib.o
	ghdl -a ${ANTB_OPTS} master_state_machine_tb.vhdl
	ghdl -e ${EL_OPTS} master_state_machine_tb
	ghdl -r ${EL_OPTS} master_state_machine_tb --vcd=master_state_machine_tb.vcd

fifo_memory_tb.vcd: fifo_memory_tb.vhdl work/polyamplib.o
	ghdl -a ${ANTB_OPTS} fifo_memory_tb.vhdl
	ghdl -e ${EL_OPTS} fifo_memory_tb
	ghdl -r ${EL_OPTS} fifo_memory_tb --vcd=fifo_memory_tb.vcd

fifo_ft_memory_tb.vcd: fifo_ft_memory_tb.vhdl work/polyamplib.o
	ghdl -a ${ANTB_OPTS} fifo_ft_memory_tb.vhdl
	ghdl -e ${EL_OPTS} fifo_ft_memory_tb
	ghdl -r ${EL_OPTS} fifo_ft_memory_tb --vcd=fifo_ft_memory_tb.vcd

#ads1271_model.o: ads1271_model.vhdl
#	ghdl -a ${ANTB_OPTS} ads1271_model.vhdl
#
#command_decoder.o : command_decoder.vhdl
#	ghdl -a ${AN_OPTS} command_decoder.vhdl

ad_interface2_tb.vcd: ad_interface2_tb.vhdl work/polyamplib.o
	ghdl -a ${ANTB_OPTS} ad_interface2_tb.vhdl
	ghdl -e ${EL_OPTS} ad_interface2_tb
	ghdl -r ${EL_OPTS} ad_interface2_tb --vcd=ad_interface2_tb.vcd

testguarded.vcd: testguarded.vhdl
	ghdl -a ${ANTB_OPTS} testguarded.vhdl
	ghdl -e ${EL_OPTS} testguarded
	ghdl -r ${EL_OPTS} testguarded --vcd=testguarded.vcd

sr_piso_tb.vcd: sr_piso_tb.vhdl work/polyamplib.o
	ghdl -a ${ANTB_OPTS} sr_piso_tb.vhdl
	ghdl -e ${EL_OPTS} sr_piso_tb
	ghdl -r ${EL_OPTS} sr_piso_tb --vcd=sr_piso_tb.vcd

spi_slave_tb1.vcd: spi_slave_tb1.vhdl work/polyamplib.o
	ghdl -a ${ANTB_OPTS} spi_slave_tb1.vhdl
	ghdl -e ${EL_OPTS} spi_slave_tb1
	ghdl -r ${EL_OPTS} spi_slave_tb1 --vcd=spi_slave_tb1.vcd

spi_slave_burst_tb.vcd: spi_slave_burst_tb.vhdl work/polyamplib.o
	ghdl -a ${ANTB_OPTS} spi_slave_burst_tb.vhdl
	ghdl -e ${EL_OPTS} spi_slave_burst_tb
	ghdl -r ${EL_OPTS} spi_slave_burst_tb --vcd=spi_slave_burst_tb.vcd

master_testbench.vcd: master_testbench.vhdl work/polyamplib.o
	ghdl -a ${ANTB_OPTS} master_testbench.vhdl
	ghdl -e ${EL_OPTS} master_testbench
	ghdl -r ${EL_OPTS} master_testbench --vcd=master_testbench.vcd

master_testbench2.vcd: master_testbench2.vhdl work/polyamplib.o
	ghdl -a ${ANTB_OPTS} master_testbench2.vhdl
	ghdl -e ${EL_OPTS} master_testbench2
	ghdl -r ${EL_OPTS} master_testbench2 --vcd=master_testbench2.vcd

master_testbench3.vcd: master_testbench3.vhdl work/polyamplib.o
	ghdl -a ${ANTB_OPTS} master_testbench3.vhdl
	ghdl -e ${EL_OPTS} master_testbench3
	ghdl -r ${EL_OPTS} master_testbench3 --vcd=master_testbench3.vcd

fast_edge_detect.vcd: fast_edge_detect.vhdl work/polyamplib.o
	ghdl -a ${ANTB_OPTS} fast_edge_detect.vhdl
	ghdl -e ${EL_OPTS} fast_edge_detect
	ghdl -r ${EL_OPTS} fast_edge_detect --vcd=fast_edge_detect.vcd
srff_tb.vcd: srff.vhdl
	ghdl -a ${ANTB_OPTS} srff.vhdl
	ghdl -e ${EL_OPTS} srff_tb
	ghdl -r ${EL_OPTS} srff_tb --vcd=srff_tb.vcd

