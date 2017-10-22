#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze e.vhdl
elab_simulate e

clean

echo "Test successful"
