#! /bin/sh

. ../../testenv.sh

synth_analyze testcase3
grep -q rising_edge  syn_testcase3.vhdl

clean

echo "Test successful"
