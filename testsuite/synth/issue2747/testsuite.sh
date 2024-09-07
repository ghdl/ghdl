#! /bin/sh

. ../../testenv.sh

synth --out=verilog top.vhdl -e > syn_top.v
grep 'ram_style="distributed"' syn_top.v

synth top.vhdl -e > syn_top.vhdl
grep 'attribute ram_style' syn_top.vhdl
analyze syn_top.vhdl

echo "Test successful"
