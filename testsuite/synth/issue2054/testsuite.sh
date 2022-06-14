#! /bin/sh

. ../../testenv.sh

synth --out=verilog flip_flop.vhdl -e > syn_flip_flop.v

if grep "input  wire" syn_flip_flop.v; then
  exit 1
fi

echo "Test successful"
