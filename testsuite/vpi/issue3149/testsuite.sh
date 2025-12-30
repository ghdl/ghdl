#! /bin/sh

. ../../testenv.sh

analyze mydesign.vhdl
elab myentity

if c_compiler_is_available && ghdl_has_feature myentity vpi; then
  $GHDL --vpi-compile -v $CC -c vpi1.c
  $GHDL --vpi-link -v $CC -o vpi1.vpi vpi1.o

  add_vpi_path

  simulate myentity --vpi=./vpi1.vpi | tee myentity.out
  diff_nocr myentity.out myentity.ref

  rm -f vpi1.vpi vpi1.o
fi
clean

echo "Test successful"
