#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze reproducer.vhdl
elab_simulate reproducer

clean

echo "Test successful"
