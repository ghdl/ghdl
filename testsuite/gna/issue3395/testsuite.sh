#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze extsig.vhdl
elab_simulate extsig

clean

echo "Test successful"
