#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze mwe_pkg.vhd ent.vhdl
elab_simulate ent

clean

echo "Test successful"
