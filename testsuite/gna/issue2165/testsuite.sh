#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=93
analyze_failure e.vhdl

export GHDL_STD_FLAGS=--std=93c
analyze e.vhdl

clean

echo "Test successful"
