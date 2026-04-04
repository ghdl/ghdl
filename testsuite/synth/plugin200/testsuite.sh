#! /bin/sh

. ../../testenv.sh

synth --out=verilog test.vhdl -e > syn_test.v
if grep -F itinst_\\outp syn_test.v; then
    exit 1
fi

synth --out=verilog test2.vhdl -e > syn_test2.v
fgrep -q '.d(d_i)' syn_test2.v
fgrep -q '.q(q_o)' syn_test2.v

echo "Test successful"
