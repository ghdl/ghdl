#! /bin/sh

. ../../testenv.sh

$GHDL -s --expect-failure --std=08 tvm_pkg.vhdl

clean

echo "Test successful"
