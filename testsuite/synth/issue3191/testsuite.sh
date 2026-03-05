#! /bin/sh

. ../../testenv.sh

synth_only repro1
synth --out=verilog repro1.vhdl -e > syn_repro1.v
grep -q "g0 => 0" syn_repro1.vhdl

synth_only repro2
grep -q "g2 => 2" syn_repro2.vhdl

clean

echo "Test successful"
