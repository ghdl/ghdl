#! /bin/sh

. ../../testenv.sh

#export GHDL_STD_FLAGS=--std=08
analyze fa.vhdl
analyze_failure fa_tb.vhdl

clean

echo "Test successful"
