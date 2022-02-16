#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth_only repro_bit_oper

echo "Test successful"
