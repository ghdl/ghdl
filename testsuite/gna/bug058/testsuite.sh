#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

analyze tb.vhdl
elab_simulate tb

analyze tb2.vhdl
elab_simulate tb2

clean

echo "Test successful"
