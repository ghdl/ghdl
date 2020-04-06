#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=-fsynopsys
synth_analyze for_loop
clean

echo "Test successful"
