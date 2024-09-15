#! /bin/sh

. ../../testenv.sh

synth --out=verilog top.vhdl -e > syn_top.v
grep -F 'signed(data) >>> ' syn_top.v

echo "Test successful"
