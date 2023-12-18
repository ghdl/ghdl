#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze mwe.vhdl
elab_simulate mwe_tb
elab_simulate mwe_tb2
elab_simulate mwe_tb3
elab_simulate mwe_tb4

clean

echo "Test successful"
