#! /bin/sh

. ../../testenv.sh

synth testcase.vhdl -e > syn_testcase.vhdl
clean

echo "Test successful"
