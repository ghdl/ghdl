#! /bin/sh

. ../../testenv.sh

synth --out=verilog zybo_top.vhdl -e > syn_zybo_top.v
grep -F '$signed(\n1.o ) * $signed(\n2.o )' syn_zybo_top.v

echo "Test successful"
