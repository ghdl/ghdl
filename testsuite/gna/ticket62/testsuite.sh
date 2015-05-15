#! /bin/sh

. ../../testenv.sh

analyze repro.vhdl
elab_simulate ent
clean

GHDL_STD_FLAGS=--std=08

analyze_failure repro.vhdl
analyze -frelaxed-rules repro.vhdl
elab_simulate -frelaxed-rules ent
clean

echo "Test successful"
