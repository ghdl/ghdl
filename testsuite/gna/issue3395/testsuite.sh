#! /bin/sh

. ../../testenv.sh

if ghdl_is_preelaboration; then
  export GHDL_STD_FLAGS=--std=08
  analyze extsig.vhdl
  elab_simulate extsig

  clean
fi

echo "Test successful"
