#! /bin/sh

. ../../testenv.sh

analyze enum_test.vhdl
elab enum_test

if ghdl_has_feature enum_test vpi; then
  if [ "$OS" = "Windows_NT" ]; then
      # Need to put the directory containing libghdlvpi.dll in the path.
      vpi_lib=`$GHDL --vpi-library-dir | sed -e 's!\\\\!/!g' -e 's!^C:!/C!g'`
      echo vpi_lib: $vpi_lib
      PATH="$PATH:$vpi_lib"
  fi

  $GHDL --vpi-compile -v gcc -c vpi_plugin.c
  $GHDL --vpi-link -v gcc -o vpi_plugin.vpi vpi_plugin.o

  simulate enum_test --vpi=./vpi_plugin.vpi > enum_test.out
  diff --strip-trailing-cr enum_test.ref enum_test.out

  rm -f vpi_plugin.vpi vpi_plugin.o
fi
clean

echo "Test successful"
