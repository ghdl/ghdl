#! /bin/sh

. ../../testenv.sh

# From issue 531

analyze repro1.vhdl
elab repro1

if c_compiler_is_available && ghdl_has_feature repro1 vpi; then
  $GHDL --vpi-compile -v gcc -c vpi1.c
  $GHDL --vpi-link -v gcc -o vpi1.vpi vpi1.o

  add_vpi_path

  simulate repro1 --vpi=./vpi1.vpi | tee repro1.out
  if grep -q Error repro1.out; then
      echo "Error in output"
      exit 1;
  fi

  rm -f vpi1.vpi vpi1.o sliced_ex.out
fi
clean

echo "Test successful"
