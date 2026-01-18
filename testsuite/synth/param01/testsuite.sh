#! /bin/sh

. ../../testenv.sh

verilog_synth_tb param02
verilog_synth_tb param01

synth_vlg2vhd param03
analyze syn_param03.vhdl tb_param03.vhdl
elab_simulate tb_param03

synth_vlg2vhd param05
analyze syn_param05.vhdl tb_param05.vhdl
elab_simulate tb_param05

echo "Test successful"
