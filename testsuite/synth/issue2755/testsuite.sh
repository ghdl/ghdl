#! /bin/sh

. ../../testenv.sh

synth --out=verilog mul_block.vhdl -e > syn_mul_block.v
grep -F '"WANNABE_STRING"' syn_mul_block.v

synth --out=vhdl mul_block.vhdl -e > syn_mul_block.vhdl
grep -F '"WANNABE_STRING"' syn_mul_block.vhdl

echo "Test successful"
