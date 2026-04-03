#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS="--std=19 -frelaxed"

synth_only tb

echo "Test successful"
