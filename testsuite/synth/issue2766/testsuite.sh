#! /bin/sh

. ../../testenv.sh

analyze --work=unisim vcomponents.vhdl ibufds.vhdl

for f in demo_comp demo_direct; do
    analyze $f.vhdl
    synth --vendor-library=unisim $f > syn_$f.vhdl
    grep -F ' work.ibufds ' syn_$f.vhdl
    synth --vendor-library=unisim --out=verilog $f > syn_$f.v
    grep -F ' ibufds ' syn_$f.v
done

clean
clean unisim

echo "Test successful"
