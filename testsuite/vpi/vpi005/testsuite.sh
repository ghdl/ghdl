#! /bin/sh

. ../../testenv.sh

analyze mydesign.vhdl
elab myentity

if c_compiler_is_available && ghdl_has_feature myentity vpi; then
  $GHDL --vpi-compile -v gcc -c vpi1.c
  $GHDL --vpi-link -v gcc -o vpi1.vpi vpi1.o

  $GHDL --vpi-compile -v gcc -c vpi2.c
  $GHDL --vpi-link -v gcc -o vpi2.vpi vpi2.o

  add_vpi_path

  simulate myentity --vpi=./vpi1.vpi --vpi=./vpi2.vpi | tee myentity.out
    if grep -q error myentity.out; then
      echo "error in output"
      exit 1;
  fi
  if ! grep -q "VPI lib 1" myentity.out; then
      echo "VPI Library 1 not loaded"
      exit 1;
  fi
  if ! grep -q "VPI lib 2" myentity.out; then
      echo "VPI Library 2 not loaded"
      exit 1;
  fi

  rm -f vpi1.vpi vpi1.o vpi2.vpi vpi2.o myentity.out
fi
clean

echo "Test successful"
