#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=-fsynopsys
synth bar.vhdl -e > syn_bar.vhdl

echo "Test successful"
