#!/bin/sh

. ../../testenv.sh

if $GHDL help coverage > /dev/null; then
    # Coverage is available
    analyze ecounter.vhdl tb1.vhdl

    rm -f coverage-*.json
    
    elab_simulate --coverage tb1

    $GHDL coverage coverage-*.json > tb1.out

    diff_nocr tb1.out tb1.ref

    clean
fi

echo "test successful"
