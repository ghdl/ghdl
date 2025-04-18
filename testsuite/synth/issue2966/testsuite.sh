#! /bin/sh

. ../../testenv.sh

synth --out=verilog repro1.vhdl -e > syn_repro1.v

if grep 'inst_b = inst_b' syn_repro1.v; then
  exit 1
fi

# Synthesize repro2
analyze repro2_pkg.vhdl
analyze repro2.vhdl
synth repro2 > syn_repro2.vhdl
synth --out=verilog repro2 > syn_repro2.v

analyze repro2b.vhdl
synth repro2b > syn_repro2b.vhdl
synth --out=verilog repro2b > syn_repro2b.v
clean

# Test vhdl output
analyze repro2_pkg.vhdl
analyze syn_repro2.vhdl
analyze syn_repro2b.vhdl

# Test verilog output
if grep 'input  b_rst' syn_repro2.v && grep 'output b_rst' syn_repro2.v; then
  exit 1
fi

echo "Test successful"
