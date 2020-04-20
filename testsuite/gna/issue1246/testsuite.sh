#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze pkg_b.vhdl pkg_a.vhdl tb.vhdl
elab_simulate tb

clean

echo "Test successful"
