#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=93
analyze write.vhd
analyze_failure wrapper.vhd

clean

export GHDL_STD_FLAGS=--std=08
analyze write.vhd
analyze_failure wrapper.vhd

clean

echo "Test successful"
