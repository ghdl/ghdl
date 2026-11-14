#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze repro.vhdl

if ghdl_is_preelaboration; then
  elab_simulate repro
fi

clean

echo "Test successful"
