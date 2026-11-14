#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze extvar.vhdl
elab_simulate extvar

clean

echo "Test successful"
