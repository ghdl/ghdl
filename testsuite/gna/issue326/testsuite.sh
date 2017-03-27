#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze foo.vhdl
analyze bar.vhdl

clean

echo "Test successful"
