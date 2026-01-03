#! /bin/sh

. ../../testenv.sh

synth_tb top

synth --out=verilog top.vhdl -e > syn_top.v
synth --out=verilog -dm -de top.vhdl -e > syn_top_dm.v
synth -dm -de top.vhdl -e > syn_top_dm.vhdl

echo "Test successful"
