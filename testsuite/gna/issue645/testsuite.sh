#! /bin/sh

#exit 0
. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze foo.vhdl
analyze tb.vhdl
elab_simulate tb

clean

echo "Test successful"
