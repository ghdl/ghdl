#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze bug.vhdl
elab_simulate bug

clean

echo "Test successful"
