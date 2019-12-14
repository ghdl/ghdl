#! /bin/sh

. ../../testenv.sh

synth function_test.vhdl -e > syn_function_test.vhdl
analyze syn_function_test.vhdl

synth repro1.vhdl -e > syn_repro1.vhdl
analyze syn_repro1.vhdl

clean

echo "Test successful"
