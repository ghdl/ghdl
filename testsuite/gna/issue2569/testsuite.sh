#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze pkg.vhdl ent.vhdl top.vhdl
elab_simulate ent_use_constant

clean

echo "Test successful"
