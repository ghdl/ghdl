#! /bin/sh

. ../../testenv.sh

synth --out=verilog test.vhdl -e > syn_test.v
if grep -F itinst_\\outp syn_test.v; then
    exit 1
fi

synth --out=verilog test2.vhdl -e > syn_test2.v
grep -F "  wire itinst_q;" syn_test2.v

echo "Test successful"
