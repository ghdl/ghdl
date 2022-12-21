#! /bin/sh

. ../../testenv.sh

analyze_failure ent.vhdl

export GHDL_STD_FLAGS=--std=08
analyze ent.vhdl

clean

echo "Test successful"
