#!/bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS="--std=08"
if ghdl_is_preelaboration; then
  synth_tb test

  clean
fi

echo "Test successful"
