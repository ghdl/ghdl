#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze t.vhdl
elab_simulate t

clean

echo "Test successful"
