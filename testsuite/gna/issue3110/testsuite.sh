#! /bin/sh

. ../../testenv.sh

if ghdl_is_preelaboration; then
  export GHDL_STD_FLAGS=--std=08
  analyze issue2.vhdl
  elab_simulate issue2

  analyze issue.vhdl
  elab_simulate issue

  clean
fi

echo "Test successful"
