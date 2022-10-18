#! /bin/sh

. ../../testenv.sh

run $GHDL -s --std=08 test.vhdl

clean

echo "Test successful"
