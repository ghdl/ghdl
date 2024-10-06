#! /bin/sh

. ../../testenv.sh

synth --out=verilog test.vhdl -e > syn_test.v
if grep -F itinst_\\outp syn_test.v; then
    exit 1
fi

echo "Test successful"
