#! /bin/sh

. ../../testenv.sh

analyze_failure repro.vhdl

export GHDL_STD_FLAGS=--std=08
analyze_failure repro.vhdl

echo "Test successful"
