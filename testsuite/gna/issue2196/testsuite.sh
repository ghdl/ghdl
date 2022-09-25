#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze_failure def_err2.vhdl

clean

echo "Test successful"
