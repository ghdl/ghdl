#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze mwe.vhdl
elab_simulate mwe

analyze mwe1.vhdl
elab_simulate mwe1 -gval=2

clean

echo "Test successful"
