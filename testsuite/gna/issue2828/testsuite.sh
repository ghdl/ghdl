#!/bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

if $GHDL help coverage > /dev/null; then
    # Coverage is available
    analyze a.vhdl

    rm -f coverage-*.json
    
    elab_simulate --coverage a --stop-time=1us
    clean
fi

echo "test successful"
