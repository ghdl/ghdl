#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth_failure mwe.vhdl -e

echo "Test successful"
