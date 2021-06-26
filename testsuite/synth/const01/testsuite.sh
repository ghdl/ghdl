#! /bin/sh

. ../../testenv.sh

synth_tb const01

# synth const02.vhdl -e > syn_const02.vhdl
synth_analyze const03
clean

echo "Test successful"
