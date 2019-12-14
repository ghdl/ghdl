#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze tb.vhd
elab_simulate tb

clean

echo "Test successful"
