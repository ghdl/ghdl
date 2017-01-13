#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze e.vhdl
elab_simulate e

analyze e2.vhdl
elab_simulate e2

clean

echo "Test successful"
