#! /bin/sh

. ../../testenv.sh

synth --out=verilog zybo_top.vhdl -e > syn_zybo_top.v
sed -e 's/(\* src="[^"]*" \*) //g' syn_zybo_top.v | grep -F '$signed(n1) * $signed(n2)'

echo "Test successful"
