#! /bin/sh

. ../../testenv.sh

analyze_failure file.vhdl

GHDL_STD_FLAGS=--std=08
$GHDL -s --std=08 file.vhdl

clean

echo "Test successful"
