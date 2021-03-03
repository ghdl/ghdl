#! /bin/sh

. ../../testenv.sh

analyze mydesign.vhdl
elab myentity

if c_compiler_is_available && ghdl_has_feature myentity vhpi; then
  $GHDL --vpi-compile -v gcc -c vhpi_lib1.c
  $GHDL --vpi-link -v gcc -o vhpi_lib1.vhpi vhpi_lib1.o
  $GHDL --vpi-compile -v gcc -c vhpi_lib2.c
  $GHDL --vpi-link -v gcc -o vhpi_lib2.vhpi vhpi_lib2.o

  add_vpi_path

  simulate myentity --vhpi=./vhpi_lib1.vhpi:my_startup --vhpi=./vhpi_lib2.vhpi:my_startup | tee myentity.out
  if grep -q Error myentity.out; then
      echo "Error in output"
      exit 1;
  fi
  if grep -q error myentity.out; then
      echo "error in output"
      exit 1;
  fi
  if ! grep -q "VHPI lib 1" myentity.out; then
      echo "VHPI Library 1 not loaded"
      exit 1;
  fi
  if ! grep -q "VHPI lib 2" myentity.out; then
      echo "VHPI Library 2 not loaded"
      exit 1;
  fi

  rm -f vhpi_lib1.vhpi vhpi_lib1.o vhpi_lib2.vhpi vhpi_lib2.o myentity.out
fi
clean

echo "Test successful"
