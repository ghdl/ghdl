#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze_failure pack.vhdl
analyze_failure --force-analysis pack.vhdl

clean

echo "Test successful"
