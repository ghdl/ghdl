#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=-fsynopsys

synth_only toint01

echo "Test successful"
