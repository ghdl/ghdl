#! /bin/sh

. ../../testenv.sh

analyze adder.vhdl
elab adder

if c_compiler_is_available && ghdl_has_feature adder vpi; then
  add_vpi_path

  $GHDL --vpi-compile -v gcc -c vpi_plugin.c
  $GHDL --vpi-link -v gcc -o vpi_plugin.vpi vpi_plugin.o

  simulate adder --vpi=./vpi_plugin.vpi

  rm -f vpi_plugin.vpi vpi_plugin.o
fi
clean

echo "Test successful"
