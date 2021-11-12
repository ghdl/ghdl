#! /bin/sh

. ../../testenv.sh

analyze blackbox1_adder.vhdl blackbox1.vhdl tb_blackbox1.vhdl
elab_simulate tb_blackbox1
clean

synth blackbox1.vhdl -e > syn_blackbox1.vhdl
analyze blackbox1_adder.vhdl syn_blackbox1.vhdl tb_blackbox1.vhdl
elab_simulate tb_blackbox1
clean

echo "Test successful"
