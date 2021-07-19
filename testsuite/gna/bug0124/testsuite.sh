#! /bin/sh

. ../../testenv.sh

$GHDL -s --std=08 repro.vhdl
$GHDL -s --std=08 repro2.vhdl

echo "Test successful"
