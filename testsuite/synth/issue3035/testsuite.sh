#! /bin/sh

. ../../testenv.sh

synth top.vhdl -e > syn_top.vhdl
grep -q "dont_touch of top" syn_top.vhdl

synth --out=verilog top.vhdl -e > syn_top.v
grep "dont_touch" syn_top.v | grep -q module

echo "Test successful"
