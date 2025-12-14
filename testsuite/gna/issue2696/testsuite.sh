#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze type_pkg.vhdl
clean

analyze type_pkg2.vhdl
clean

echo "Test successful"
