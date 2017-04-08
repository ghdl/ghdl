#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze atod.vhdl
elab_simulate atod

clean

echo "Test successful"
