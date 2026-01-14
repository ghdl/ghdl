#! /bin/sh

. ../../testenv.sh

if ghdl_is_preelaboration; then
    export GHDL_STD_FLAGS=--std=08
    analyze test.vhdl test.psl
    $GHDL -m $GHDL_STD_FLAGS test

    clean
fi

echo "Test successful"
