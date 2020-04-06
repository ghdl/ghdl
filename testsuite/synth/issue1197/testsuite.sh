#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=-fsynopsys
synth_analyze generics_1
clean

echo "Test successful"
