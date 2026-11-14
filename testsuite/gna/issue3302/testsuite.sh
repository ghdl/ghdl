#! /bin/sh

. ../../testenv.sh


export GHDL_STD_FLAGS=--std=08
analyze a.vhdl
elab_simulate c

analyze a2.vhdl
elab_simulate_failure c2

clean


echo "Test successful"
