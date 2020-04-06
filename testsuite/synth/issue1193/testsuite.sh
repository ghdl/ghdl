#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=-fsynopsys
synth_analyze counters_7

clean

echo "Test successful"
