#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze bug.vhdl
elab_simulate tb

clean

echo "Test successful"
