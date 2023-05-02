#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=93
analyze_failure pkg.vhdl

export GHDL_STD_FLAGS=--std=93c
analyze pkg.vhdl
analyze ent.vhdl
elab_simulate ent

clean

echo "Test successful"
