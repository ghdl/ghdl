#! /bin/sh

. ../../testenv.sh

# ent.vhdl
synth ent.vhdl -e > syn_ent.vhdl
grep -q "dont_touch of rdaddr" syn_ent.vhdl

synth --out=raw-vhdl ent.vhdl -e > syn_ent2.vhdl
grep -q "dont_touch of rdaddr" syn_ent2.vhdl

synth --out=verilog ent.vhdl -e > syn_ent.v
grep "dont_touch" syn_ent.v | grep -q rdaddr

# inp.vhdl
synth --out=verilog inp.vhdl -e > syn_inp.v
grep "dont_touch" syn_inp.v | grep -q "input  d"
synth --out=raw-vhdl inp.vhdl -e > syn_inp.vhdl
grep "dont_touch of d" syn_inp.vhdl

# outp.vhdl
synth --out=verilog outp.vhdl -e > syn_outp.v
grep "dont_touch" syn_outp.v | grep -q "output q"

synth --out=raw-vhdl outp.vhdl -e > syn_outp.vhdl
grep "dont_touch of q" syn_outp.vhdl

# attr2
synth --out=raw-vhdl attr2.vhdl -e > syn_attr2.vhdl
grep "dont_touch of q" syn_attr2.vhdl
grep "dont_touch of d" syn_attr2.vhdl
analyze syn_attr2.vhdl

clean

echo "Test successful"
