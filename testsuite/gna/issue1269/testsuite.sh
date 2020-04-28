#! /bin/sh

. ../../testenv.sh

analyze_failure ent93.vhdl

export GHDL_STD_FLAGS=--std=08
analyze_failure ent.vhdl

echo "Test successful"
