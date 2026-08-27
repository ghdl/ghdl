#! /bin/sh

. ../../testenv.sh

if ghdl_is_preelaboration; then
  export GHDL_STD_FLAGS=--std=08
  analyze extvar.vhdl
  elab_simulate extvar

  clean
fi

echo "Test successful"
