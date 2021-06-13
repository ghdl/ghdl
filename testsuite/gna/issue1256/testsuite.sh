#! /bin/sh

. ../../testenv.sh

analyze enum_test.vhdl
elab enum_test

if c_compiler_is_available && ghdl_has_feature enum_test vpi; then
  add_vpi_path

  $GHDL --vpi-compile -v gcc -c vpi_plugin.c
  $GHDL --vpi-link -v gcc -o vpi_plugin.vpi vpi_plugin.o

  simulate enum_test --vpi=./vpi_plugin.vpi > enum_test.out
  diff_nocr enum_test.ref enum_test.out

  rm -f vpi_plugin.vpi vpi_plugin.o
fi
clean

echo "Test successful"
