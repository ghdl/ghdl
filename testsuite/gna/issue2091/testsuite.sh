#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze log.vhdl test.vhdl
elab_simulate test

clean

echo "Test successful"
