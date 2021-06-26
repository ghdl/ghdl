#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth_tb conv01

echo "Test successful"
