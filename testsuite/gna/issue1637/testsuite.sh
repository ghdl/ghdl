#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze show_bug.vhdl
elab_simulate show_bug

clean

echo "Test successful"
