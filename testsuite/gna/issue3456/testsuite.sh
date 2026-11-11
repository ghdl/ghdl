#! /bin/sh

. ../../testenv.sh

if ghdl_is_preelaboration; then
  export GHDL_STD_FLAGS=--std=08
  analyze generic_entity.vhdl generic_bug.vhdl
  elab_simulate generic_bug

  clean
fi

echo "Test successful"
