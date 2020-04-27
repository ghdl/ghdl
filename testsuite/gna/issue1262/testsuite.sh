#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze pkg_slv.vhdl
analyze ent.vhdl
analyze tb2.vhdl
elab_simulate tb2

analyze tb.vhdl
elab_simulate tb

clean

echo "Test successful"
