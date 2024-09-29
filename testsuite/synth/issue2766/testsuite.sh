#! /bin/sh

. ../../testenv.sh

analyze --work=unisim unisim.vhdl
analyze demo_comp.vhdl
synth --vendor-library=unisim demo_comp > syn_demo_comp.vhdl
grep -F ' work.ibufds ' syn_demo_comp.vhdl
synth --vendor-library=unisim --out=verilog demo_comp > syn_demo_comp.v
grep -F ' ibufds ' syn_demo_comp.v

clean
clean unisim

echo "Test successful"
