#!/bin/sh

. ../../testenv.sh

if $GHDL help coverage > /dev/null; then
    # Coverage is available
    analyze a.vhdl c.vhdl e.vhdl

    rm -f coverage-*.json
    
    elab_simulate --coverage a
    elab_simulate --coverage c
    elab_simulate --coverage e

    clean
fi

echo "test successful"
