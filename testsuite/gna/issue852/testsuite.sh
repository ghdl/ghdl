#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze tb.vhdl
elab recordOfRecord_tb
if ghdl_has_feature recordofrecord_tb ghw; then
  simulate recordOfRecord_tb --wave=recordOfRecord_tb.ghw
else
  simulate recordOfRecord_tb
fi
clean

echo "Test successful"
