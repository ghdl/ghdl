#! /bin/sh

. ../../testenv.sh

synth --out=verilog top.vhdl -e > syn_top.v
grep -F '$signed(data_numer_i) / $signed(data_denom_i)' syn_top.v

echo "Test successful"
