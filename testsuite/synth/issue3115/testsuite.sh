#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth --keep-hierarchy=no repro_in.vhdl -e > syn_repro_in.vhdl
fgrep -q "attribute syn_keep of dut_a_n1" syn_repro_in.vhdl

synth --keep-hierarchy=no repro_out.vhdl -e > syn_repro_out.vhdl
fgrep -q "attribute syn_keep of dut_o_n2" syn_repro_out.vhdl

echo "Test successful"
