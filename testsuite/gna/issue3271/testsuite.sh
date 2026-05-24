#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=19
analyze_failure ent.vhdl
analyze_failure ent2.vhdl

clean

echo "Test successful"
