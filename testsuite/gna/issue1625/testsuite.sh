#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze level2.vhdl
analyze level1.vhdl
elab_simulate level1

analyze level0.vhdl
elab_simulate level0

clean

echo "Test successful"
