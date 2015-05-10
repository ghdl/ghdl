#! /bin/sh

. ../../testenv.sh

analyze --std=08 ent.vhdl
analyze_failure --std=08 ent2.vhdl

GHDL_STD_FLAGS=--std=08 clean

echo "Test successful"
