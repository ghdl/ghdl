#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=19
analyze axis_pkg.vhdl
analyze ent.vhdl

clean

echo "Test successful"
