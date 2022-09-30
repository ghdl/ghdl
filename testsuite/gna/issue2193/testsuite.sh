#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze demo2.vhdl
elab_simulate demo2

analyze demo.vhdl
elab_simulate_failure demo

clean

echo "Test successful"
