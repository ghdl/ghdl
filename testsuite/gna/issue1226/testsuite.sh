#! /bin/sh

. ../../testenv.sh

analyze adder.vhdl
elab adder

if ghdl_has_feature adder vpi; then
  if [ "$OS" = "Windows_NT" ]; then
      # Need to put the directory containing libghdlvpi.dll in the path.
      vpi_lib=`$GHDL --vpi-library-dir | sed -e 's!\\\\!/!g' -e 's!^C:!/C!g'`
      echo vpi_lib: $vpi_lib
      PATH="$PATH:$vpi_lib"
  fi

  $GHDL --vpi-compile -v gcc -c vpi_plugin.c
  $GHDL --vpi-link -v gcc -o vpi_plugin.vpi vpi_plugin.o

  simulate adder --vpi=./vpi_plugin.vpi

  rm -f vpi_plugin.vpi vpi_plugin.o
fi
clean

echo "Test successful"
