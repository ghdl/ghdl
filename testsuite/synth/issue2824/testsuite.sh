#! /bin/sh

. ../../testenv.sh

synth_failure top.vhdl -e
synth_only top2

echo "Test successful"
