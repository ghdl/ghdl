#! /bin/sh

. ../../testenv.sh

analyze mydesign.vhdl
elab myentity

if c_compiler_is_available && ghdl_has_feature myentity vhpi; then
  $GHDL --vpi-compile -v gcc -c vhpi_lib.c
  $GHDL --vpi-link -v gcc -o vhpi_lib.vhpi vhpi_lib.o

  add_vpi_path

  simulate myentity --vhpi=./vhpi_lib.vhpi --vhpi-trace=./vhpi_trace.log | tee myentity.out
  if grep -q Error myentity.out; then
      echo "Error in output"
      exit 1;
  fi
  if grep -q error myentity.out; then
      echo "error in output"
      exit 1;
  fi
  if ! grep -q "vhpi_register_cb (" vhpi_trace.log; then
      echo "VHPI trace missing"
      exit 1;
  fi

  rm -f vhpi_lib.vhpi vhpi_lib.o myentity.out vhpi_trace.log
fi
clean

echo "Test successful"
