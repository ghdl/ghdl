#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze pkg2.vhdl
analyze top2.vhdl
elab_simulate top2

analyze pkg.vhdl
analyze top.vhdl
elab_simulate top

clean

echo "Test successful"
