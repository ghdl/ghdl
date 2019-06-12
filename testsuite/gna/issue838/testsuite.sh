#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
analyze_failure top.vhdl
analyze -frelaxed-rules top.vhdl

clean

echo "Test successful"
