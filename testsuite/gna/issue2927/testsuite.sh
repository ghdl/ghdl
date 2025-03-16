#!/bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

if $GHDL help coverage > /dev/null; then
    # Coverage is available
    analyze reproducer.vhdl tb_reproducer.vhdl

    rm -f coverage-*.json
    
    elab_simulate --coverage --coverage-output=tb_reproducer.json tb_reproducer

    $GHDL coverage --format=lcov tb_reproducer.json > reproducer.lcov

    clean
fi

echo "test successful"
