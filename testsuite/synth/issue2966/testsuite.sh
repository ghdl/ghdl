#! /bin/sh

. ../../testenv.sh

synth --out=verilog repro1.vhdl -e > syn_repro1.v

if grep 'inst_b = inst_b' syn_repro1.v; then
  exit 1
fi

# Check valid verilog
synth syn_repro1.v -e > /dev/null

# Synthesize repro2
analyze repro2_pkg.vhdl
analyze repro2.vhdl
synth repro2 > syn_repro2.vhdl
synth --out=verilog repro2 > syn_repro2.v
synth syn_repro2.v -e > /dev/null

analyze repro2b.vhdl
synth repro2b > syn_repro2b.vhdl
synth --out=verilog repro2b > syn_repro2b.v
synth syn_repro2b.v -e > /dev/null

clean

# Test vhdl output
analyze repro2_pkg.vhdl
analyze syn_repro2.vhdl
analyze syn_repro2b.vhdl

# Test verilog output
if grep 'input  b_rst' syn_repro2.v && grep 'output b_rst' syn_repro2.v; then
  exit 1
fi

synth repro3.vhdl -e > syn_repro3.vhdl
analyze syn_repro3.vhdl

echo "Test successful"
