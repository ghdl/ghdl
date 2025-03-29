#!/bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

if $GHDL help coverage > /dev/null; then
    # Coverage is available

    rm -rf subdir*
    mkdir subdir
    # Either a real subdir on windows, or a normal file on linux
    cp repro.vhdl subdir\\repro.vhdl
    
    analyze subdir\\repro.vhdl

    rm -f coverage-*.json
    
    elab_simulate --coverage --coverage-output=tb.json json_format

    $GHDL coverage --format=lcov tb.json > res.lcov

    clean
    rm -rf subdir*
fi

echo "test successful"
