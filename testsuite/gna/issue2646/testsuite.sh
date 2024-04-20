#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

analyze ex1.vhdl
elab_simulate ex1

analyze ex2.vhdl
elab_simulate_failure ex2

analyze ex3.vhdl
elab_simulate_failure ex3

clean

echo "Test successful"
