#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

synth_failure not_full.vhdl -e

echo "Test successful"
