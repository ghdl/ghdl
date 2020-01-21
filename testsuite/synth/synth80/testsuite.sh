#! /bin/sh

. ../../testenv.sh

synth testcase.vhdl -e > syn_testcase.vhdl
synth test2.vhdl -e > syn_test2.vhdl
clean

echo "Test successful"
