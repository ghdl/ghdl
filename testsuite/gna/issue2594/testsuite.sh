#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze test.vhdl
elab_simulate selsigass_tb

analyze tb2.vhdl
elab_simulate tb2

clean

echo "Test successful"
