#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS='--std=08 -fpsl'
analyze_failure top.vhd

clean

echo "Test successful"
