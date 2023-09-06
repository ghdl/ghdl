#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze bug.vhdl

analyze bug2.vhdl
elab_simulate bug2

analyze bug3.vhdl
elab_simulate bug3

clean

echo "Test successful"
