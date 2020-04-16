#! /bin/sh

. ../../testenv.sh

synth_failure top.vhdl -e
synth_failure top3.vhdl -e

echo "Test successful"
