#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze e3.vhdl
elab_simulate e3

analyze e2.vhdl
elab_simulate e2

analyze_failure ent.vhdl

# Not yet fixed
# analyze e.vhdl

clean

echo "Test successful"
