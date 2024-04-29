#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze test_others.vhdl

clean

echo "Test successful"
