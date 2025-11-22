#! /bin/sh

. ../../testenv.sh

if ghdl_is_preelaboration; then
  export GHDL_STD_FLAGS=--std=08
  analyze issue2.vhdl
  elab_simulate issue2

  clean
fi

echo "Test successful"
