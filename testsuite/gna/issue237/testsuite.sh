#! /bin/sh

. ../../testenv.sh

analyze test_array.vhdl
elab test_array

if c_compiler_is_available && ghdl_has_feature test_array vpi; then
  add_vpi_path

  $GHDL --vpi-compile -v gcc $CFLAGS -c vpi1.c
  $GHDL --vpi-link -v gcc $CFLAGS -o vpi1.vpi vpi1.o

  simulate test_array --vpi=./vpi1.vpi

  rm -f vpi1.vpi vpi1.o
fi
clean

echo "Test successful"
