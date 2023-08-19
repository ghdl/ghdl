#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=93
synth_failure io_pin.vhdl -e

echo "Test successful"
