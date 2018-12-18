#!/bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS="--std=08"
analyze all08.vhdl

clean

echo "test successful"
