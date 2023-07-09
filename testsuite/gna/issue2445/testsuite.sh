#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze lfsr.vhdl

analyze blk.vhdl
elab_simulate blk

clean

echo "Test successful"
