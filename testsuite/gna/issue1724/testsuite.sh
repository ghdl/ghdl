#! /bin/sh

. ../../testenv.sh

$GHDL -s --std=08 const_test.vhdl const_test_vunit.vhdl
$GHDL -s --std=08 const_test.vhdl const_test_vunit2.vhdl

clean

echo "Test successful"
