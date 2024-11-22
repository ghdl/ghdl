#! /bin/sh

. ../../testenv.sh

analyze dut.vhdl
elab dut

if c_compiler_is_available && ghdl_has_feature dut vpi; then
  $GHDL --vpi-compile -v $CC -c vpi1.c -g
  $GHDL --vpi-link -v $CC -o vpi1.vpi vpi1.o

  add_vpi_path

  simulate dut --vpi=./vpi1.vpi | tee dut.out
  if grep -q ERROR dut.out; then
      echo "Error in output"
      exit 1;
  fi

  rm -f vpi1.vpi vpi1.o dut.out
fi
clean

echo "Test successful"
