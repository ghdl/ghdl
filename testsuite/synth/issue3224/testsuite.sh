#! /bin/sh

. ../../testenv.sh

synth_only foo

grep -q -F 'attribute test of b0' syn_foo.vhdl

synth --out=verilog foo.vhdl -e > syn_foo.v
grep -q -F ' (* test="yes" *) bar b0' syn_foo.v

synth --out=verilog attr2.vhdl -e > syn_attr2.v
grep -q -F ' (* test="yes" *) attr2_sub_Bbehav b0' syn_attr2.v

synth --out=verilog attr3.vhdl -e > syn_attr3.v
grep -q -F ' (* test="yes" *) attr3_sub_Bbehav b0' syn_attr3.v

echo "Test successful"
