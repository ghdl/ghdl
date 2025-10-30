#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth --out=verilog ent.vhdl -e > syn_ent.v
fgrep -q 'assign my_out_port =' syn_ent.v

synth --out=verilog ent2.vhdl -e > syn_ent2.v
fgrep -q 'assign my_out_port =' syn_ent2.v

echo "Test successful"
