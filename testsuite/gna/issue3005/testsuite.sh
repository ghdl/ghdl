#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze f.vhdl
elab_simulate f

clean

echo "Test successful"
