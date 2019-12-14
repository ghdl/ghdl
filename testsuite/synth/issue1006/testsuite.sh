#! /bin/sh

. ../../testenv.sh

synth test_package.vhdl test.vhdl -e test > syn_test.vhdl
analyze test_package.vhdl syn_test.vhdl
clean

echo "Test successful"
