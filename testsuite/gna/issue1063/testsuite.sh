#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze matching.vhdl
elab_simulate matching

clean

echo "Test successful"
