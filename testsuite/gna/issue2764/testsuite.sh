#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=93c
analyze tb.vhdl
elab_simulate tb

clean

echo "Test successful"
