#! /bin/sh

. ../../testenv.sh

analyze ent.vhdl
elab ent

if c_compiler_is_available && ghdl_has_feature ent vpi; then
  add_vpi_path

  $GHDL --vpi-compile -v $CC -c vpi_plugin.c
  $GHDL --vpi-link -v $CC -o vpi_plugin.vpi vpi_plugin.o

  simulate ent --vpi=./vpi_plugin.vpi

  rm -f vpi_plugin.vpi vpi_plugin.o
fi
clean

echo "Test successful"
