#! /bin/sh

. ../../testenv.sh

if ghdl_is_preelaboration; then
  export GHDL_STD_FLAGS=--std=08
  analyze a.vhdl
  elab_simulate tb_ghdl

  clean

  analyze a2.vhdl
  elab_simulate tb_ghdl
  
  clean

  analyze b.vhdl
  elab_simulate tb_ghdl
 
  clean
fi

echo "Test successful"
