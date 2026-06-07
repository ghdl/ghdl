#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze mwe.vhdl
elab_simulate mwe

analyze mwe2.vhdl
elab_simulate mwe2

clean

echo "Test successful"
