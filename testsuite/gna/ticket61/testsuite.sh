#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

analyze bug.vhdl
elab_simulate ent
clean

echo "Test successful"
