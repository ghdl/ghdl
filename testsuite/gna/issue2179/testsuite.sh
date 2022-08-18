#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS="--std=08"
analyze_failure e.vhdl

export GHDL_STD_FLAGS="--std=08 -frelaxed"
analyze e.vhdl

clean

echo "Test successful"
