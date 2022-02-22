#! /bin/sh

exit 0
. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth_only bug

echo "Test successful"
