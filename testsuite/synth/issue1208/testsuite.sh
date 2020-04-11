#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=-fsynopsys

synth adders_4.vhdl -e > syn_adders_4.vhdl

echo "Test successful"
