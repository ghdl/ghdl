#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze testing.vhdl
elab_simulate testing

clean

echo "Test successful"
