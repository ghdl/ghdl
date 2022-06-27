#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth --out=verilog -Wno-nowrite a.vhdl -e > syn_a.v

if grep channel syn_a.v; then
  exit 1
fi

echo "Test successful"
