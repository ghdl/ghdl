#! /bin/sh

. ../../testenv.sh

if ghdl_is_preelaboration; then
  export GHDL_STD_FLAGS=--std=08
  analyze test_qual.vhdl
  elab_simulate test_qual

  clean
fi

echo "Test successful"
