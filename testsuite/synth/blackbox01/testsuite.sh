#! /bin/sh

. ../../testenv.sh

# The testbench
analyze blackbox1_adder.vhdl blackbox1.vhdl tb_blackbox1.vhdl
elab_simulate tb_blackbox1
clean

# Synthesize using a not bounded component
synth blackbox1.vhdl -e > syn_blackbox1.vhdl
analyze blackbox1_adder.vhdl syn_blackbox1.vhdl tb_blackbox1.vhdl
elab_simulate tb_blackbox1
clean

# Synthesize using entity + syn_black_box attribute
synth blackbox1_adder_bb.vhdl blackbox1.vhdl -e > syn_blackbox1.vhdl
analyze blackbox1_adder.vhdl syn_blackbox1.vhdl tb_blackbox1.vhdl
elab_simulate tb_blackbox1
clean


echo "Test successful"
