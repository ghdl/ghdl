#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
analyze file.vhdl
elab_simulate ent

analyze bug2.vhdl

clean

echo "Test successful"
