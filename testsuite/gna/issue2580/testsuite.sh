#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze test_pkg.vhdl
analyze test1.vhdl
elab_simulate test1

analyze test2.vhdl
elab_simulate test2

if ghdl_is_preelaboration; then
    analyze streamtb.vhdl
    elab_simulate streamtb
fi

clean

echo "Test successful"
