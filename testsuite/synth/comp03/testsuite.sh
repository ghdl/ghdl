#! /bin/sh

. ../../testenv.sh

synth_failure sub1.vhdl comp03.vhdl -e
synth_failure sub2.vhdl comp04.vhdl -e


echo "Test successful"
