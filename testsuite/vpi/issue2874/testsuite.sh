#! /bin/sh

. ../../testenv.sh

analyze mwe.vhdl
elab mwe

if c_compiler_is_available && ghdl_has_feature mwe vpi; then
  $GHDL --vpi-compile -v $CC -c mwe_vpi.c -g
  $GHDL --vpi-link -v $CC -o mwe_vpi.vpi mwe_vpi.o

  add_vpi_path

  simulate mwe --vpi=./mwe_vpi.vpi

  rm -f mwe_vpi.vpi mwe_vpi.o
fi
clean

echo "Test successful"

