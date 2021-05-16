#! /bin/sh

. ../../testenv.sh

analyze ent93.vhdl

clean

export GHDL_STD_FLAGS=--std=08
analyze ent.vhdl

clean

echo "Test successful"
