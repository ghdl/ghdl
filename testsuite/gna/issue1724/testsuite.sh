#! /bin/sh

. ../../testenv.sh

$GHDL -s --std=08 const_test.vhdl const_test_vunit.vhdl

clean

echo "Test successful"
