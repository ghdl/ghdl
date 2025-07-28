#! /bin/sh

. ../../testenv.sh

synth ent.vhdl -e > syn_ent.vhdl
grep -q "dont_touch of rdaddr" syn_ent.vhdl

synth --out=verilog ent.vhdl -e > syn_ent.v
grep "dont_touch" syn_ent.v | grep -q rdaddr

synth --out=verilog inp.vhdl -e > syn_inp.v
grep "dont_touch" syn_inp.v | grep -q "input  d"
synth --out=verilog outp.vhdl -e > syn_outp.v
grep "dont_touch" syn_outp.v | grep -q "output q"

echo "Test successful"
