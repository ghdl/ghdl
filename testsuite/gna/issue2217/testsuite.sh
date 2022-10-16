#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze test1.vhdl
analyze test2.vhdl
analyze repro.vhdl

clean

echo "Test successful"
