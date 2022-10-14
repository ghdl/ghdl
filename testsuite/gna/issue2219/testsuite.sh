#! /bin/sh

. ../../testenv.sh

run $GHDL -s --std=08 ent.vhdl

clean

echo "Test successful"
