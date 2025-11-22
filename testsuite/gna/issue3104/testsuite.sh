#! /bin/sh

. ../../testenv.sh

if ghdl_is_preelaboration; then
  export GHDL_STD_FLAGS=--std=08
  analyze tb.vhdl
  elab_simulate tb_ghdl

  clean
fi

echo "Test successful"
