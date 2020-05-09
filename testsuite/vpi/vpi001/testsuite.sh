#! /bin/sh

. ../../testenv.sh

analyze mydesign.vhdl
elab myentity

if ghdl_has_feature myentity vpi; then
  $GHDL --vpi-compile -v gcc -c vpi1.c
  $GHDL --vpi-link -v gcc -o vpi1.vpi vpi1.o

  add_vpi_path

  simulate myentity --vpi=./vpi1.vpi | tee myentity.out
  if grep -q Error myentity.out; then
      echo "Error in output"
      exit 1;
  fi

  rm -f vpi1.vpi vpi1.o myentity.out
fi
clean

echo "Test successful"
