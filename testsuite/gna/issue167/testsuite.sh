#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
analyze pkg1.vhdl
analyze pkg2.vhdl
clean

echo "Test successful"
