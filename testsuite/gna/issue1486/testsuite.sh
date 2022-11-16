#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze testcasestatic.vhdl

clean

echo "Test successful"
