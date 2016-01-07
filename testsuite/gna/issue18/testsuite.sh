#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
analyze fum.vhdl
analyze integer_ambig.vhdl
elab_simulate  overload_index_issue

clean

echo "Test successful"
