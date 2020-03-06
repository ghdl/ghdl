#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze_failure ent.vhdl
analyze -frelaxed ent.vhdl

clean

echo "Test successful"
