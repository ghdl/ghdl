#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze mwe.vhdl
elab_simulate mwe

analyze mwe3.vhdl
elab_simulate mwe3

clean

echo "Test successful"
