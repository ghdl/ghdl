#! /bin/sh

. ../../testenv.sh

$GHDL -s --std=08 repro.vhdl

echo "Test successful"
