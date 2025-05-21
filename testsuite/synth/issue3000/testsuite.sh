#! /bin/sh

. ../../testenv.sh

synth --out=verilog memory.vhdl -e > syn_memory.v

echo "Test successful"
