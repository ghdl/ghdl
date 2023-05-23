#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=93
analyze_failure pkg.vhdl

GHDL_STD_FLAGS=--std=93c
analyze pkg.vhdl
analyze ent.vhdl
elab_simulate ent

clean

GHDL_STD_FLAGS=--std=08
analyze ent2.vhdl
elab_simulate ent

clean

echo "Test successful"
