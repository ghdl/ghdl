#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=93
analyze_failure pkg.vhdl

export GHDL_STD_FLAGS=--std=93c
analyze pkg.vhdl

clean

echo "Test successful"
