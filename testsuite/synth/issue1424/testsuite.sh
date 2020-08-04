#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS="--std=08 -fsynopsys"
synth_analyze bar
clean

echo "Test successful"
