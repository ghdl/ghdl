#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze package.vhdl

clean

echo "Test successful"
