#! /bin/sh

. ../../testenv.sh

synth --out=verilog flip_flop.vhdl -e > syn_flip_flop.v
if grep "input  wire" syn_flip_flop.v; then
  exit 1
fi

synth --out=verilog testcase2.vhdl -e > syn_testcase2.v
if grep "assign edge" syn_testcase2.v; then
  exit 1
fi

synth --out=verilog testcase3.vhdl -e > syn_testcase3.v
if grep "edge =" syn_testcase3.v; then
  exit 1
fi

echo "Test successful"
