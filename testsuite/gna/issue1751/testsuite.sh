#! /bin/sh

. ../../testenv.sh

$GHDL -s --std=08 ent.vhdl

clean --std=08

echo "Test successful"
