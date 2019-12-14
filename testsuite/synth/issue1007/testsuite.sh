#! /bin/sh

. ../../testenv.sh

synth test_entity.vhdl test_wrapper.vhdl -e > syn_test_wrapper.vhdl
analyze syn_test_wrapper.vhdl
clean

echo "Test successful"
