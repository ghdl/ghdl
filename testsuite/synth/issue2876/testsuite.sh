#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

synth_tb record_bug

synth_tb record_bug2

echo "Test successful"
