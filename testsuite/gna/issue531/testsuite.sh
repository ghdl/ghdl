#! /bin/sh

. ../../testenv.sh

analyze repro1.vhdl
elab sliced_ex

if c_compiler_is_available && ghdl_has_feature sliced_ex vpi; then
  $GHDL --vpi-compile -v gcc -c vpi1.c
  $GHDL --vpi-link -v gcc -o vpi1.vpi vpi1.o

  add_vpi_path

  simulate sliced_ex --vpi=./vpi1.vpi | tee sliced_ex.out
  if grep -q Error sliced_ex.out; then
      echo "Error in output"
      exit 1;
  fi

  rm -f vpi1.vpi vpi1.o sliced_ex.out
fi
clean

echo "Test successful"
