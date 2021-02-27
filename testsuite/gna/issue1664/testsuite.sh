#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze_failure repro.vhdl
analyze_failure ppm.vhdl

clean

echo "Test successful"
