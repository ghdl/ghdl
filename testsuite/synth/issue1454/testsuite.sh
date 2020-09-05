#! /bin/sh

. ../../testenv.sh

synth_analyze dummy_top
grep -q dummy_sub_inst syn_dummy_top.vhdl

clean

echo "Test successful"
