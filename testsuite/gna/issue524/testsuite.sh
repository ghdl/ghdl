#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze_failure spi2apb.vhdl

analyze cond.vhdl

clean

echo "Test successful"
