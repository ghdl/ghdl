#! /bin/sh

. ../../testenv.sh

synth --out=verilog bug.vhdl -e > syn_bug.v

if grep val syn_bug.v; then
  exit 1
fi

echo "Test successful"
