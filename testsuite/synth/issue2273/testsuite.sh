#! /bin/sh

. ../../testenv.sh

synth -fsynopsys --out=verilog @mc8051.f -e mc8051_core > mc8051.v

echo "Test successful"
