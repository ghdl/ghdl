#! /bin/sh

. ../../testenv.sh

synth_tb top_module

clean

synth --out=verilog top_module.vhdl -e > syn_top_orig.v

sed -e 's/\(n[0-9]\)/n\1/g' < syn_top_orig.v > syn_top.v

synth syn_top.v -e > syn_top.vhdl

analyze syn_top.vhdl tb_top_module.vhdl

elab_simulate tb_top_module
clean

echo "Test successful"
