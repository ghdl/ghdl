#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze mac.vhdl
analyze_failure mac_test.vhdl
#elab_simulate mac_test

clean

echo "Test successful"
