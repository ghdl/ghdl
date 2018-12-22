#!/bin/sh

if [ "$ISGPL" = "true" ]; then
    # std_logic_1164 not available in openieee.
    echo "test skipped"
    exit 0
fi

. ../../testenv.sh

GHDL_STD_FLAGS="--std=08"
analyze all08.vhdl

clean

echo "test successful"
