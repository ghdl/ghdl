#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=93
analyze write.vhd
analyze_failure wrapper.vhd

analyze_failure nest.vhdl

clean


export GHDL_STD_FLAGS=--std=02
analyze nest.vhdl

clean

export GHDL_STD_FLAGS=--std=08
analyze write.vhd
analyze_failure wrapper.vhd

analyze_failure nest.vhdl

clean

echo "Test successful"
