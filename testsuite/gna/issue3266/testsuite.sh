#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze temp.vhdl
elab_simulate temp

clean

echo "Test successful"
