#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

analyze match_operators.vhdl
elab_simulate match_operators

clean

echo "Test successful"
