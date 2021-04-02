#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze element_bug.vhdl
elab_simulate e1_tb

clean

echo "Test successful"
