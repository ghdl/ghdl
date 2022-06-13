#! /bin/sh

. ../../testenv.sh

synth --out=verilog testcase.vhdl -e > syn_testcase.v

if grep "module testcase2" syn_testcase.v; then
  exit 1
fi

echo "Test successful"
