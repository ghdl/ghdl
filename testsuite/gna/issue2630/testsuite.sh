#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze ext_name.vhdl
elab_simulate ext_name

clean

echo "Test successful"
