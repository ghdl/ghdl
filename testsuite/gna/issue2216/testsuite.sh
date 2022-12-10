#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze pkg.vhdl ent.vhdl
elab_simulate ent

clean

echo "Test successful"
