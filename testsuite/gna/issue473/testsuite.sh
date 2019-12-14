#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze tbmem.vhdl
elab_simulate tbmem

clean

echo "Test successful"
