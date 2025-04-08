#! /bin/sh

. ../../testenv.sh

synth --out=verilog repro1.vhdl -e > syn_repro1.v

if grep 'inst_b = inst_b' syn_repro1.v; then
  exit 1
fi

echo "Test successful"
