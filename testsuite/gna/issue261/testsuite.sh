#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze_failure e.vhdl

analyze e1.vhdl
elab_simulate e

clean

echo "Test successful"
