#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

analyze matching_case.vhdl

clean

echo "Test successful"
