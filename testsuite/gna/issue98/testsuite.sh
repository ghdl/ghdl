#! /bin/sh

. ../../testenv.sh

analyze test_load.vhdl
elab test_load

if ghdl_has_feature test_load vpi; then
  if [ "$OS" = "Windows_NT" ]; then
      vpi_lib=`$GHDL --vpi-library-dir | sed -e 's!\\\\!/!g' -e 's!^C:!/C!g'`
      echo vpi_lib: $vpi_lib
      PATH="$PATH:$vpi_lib"
  fi

  $GHDL --vpi-compile -v gcc -c vpi1.c
  $GHDL --vpi-link -v gcc -o vpi1.vpi vpi1.o

  simulate test_load --vpi=./vpi1.vpi

  rm -f vpi1.vpi vpi1.o
fi
clean

echo "Test successful"
