#! /bin/sh

. ../../testenv.sh

synth --out=verilog attr.vhdl -e > syn_attr.v

grep "MY_ENTITY_ATTRIBUTE=" syn_attr.v

echo "Test successful"
