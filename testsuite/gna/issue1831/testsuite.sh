#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze sipo.vhdl
elab_simulate sipo

clean

echo "Test successful"
