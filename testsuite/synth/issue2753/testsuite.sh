#! /bin/sh

. ../../testenv.sh

synth --out=verilog zybo_top.vhdl -e > syn_zybo_top.v
grep -F '$signed(n1_o) * $signed(n2_o)' syn_zybo_top.v

echo "Test successful"
