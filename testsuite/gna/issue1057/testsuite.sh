#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze pkg.vhdl pkg2.vhdl

clean

echo "Test successful"
