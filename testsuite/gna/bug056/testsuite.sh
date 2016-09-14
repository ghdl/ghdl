#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

analyze tb.vhdl
elab_simulate tb

clean

echo "Test successful"
