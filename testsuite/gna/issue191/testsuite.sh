#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
# Cannot yet generate code.
$GHDL -s --std=08 repro.vhdl

clean

echo "Test successful"
