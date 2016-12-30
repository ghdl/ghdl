#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze test.vhdl
elab_simulate test

analyze rec.vhdl
elab_simulate rec

analyze arr.vhdl
elab_simulate arr

clean

echo "Test successful"
