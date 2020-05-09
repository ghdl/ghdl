#! /bin/sh

. ../../testenv.sh

analyze mydesign.vhdl
elab myentity

if ghdl_has_feature myentity vpi; then
  if [ "$OS" = "Windows_NT" ]; then
      vpi_lib=`$GHDL --vpi-library-dir | sed -e 's!\\\\!/!g' -e 's!^C:!/C!g'`
      echo vpi_lib: $vpi_lib
      PATH="$PATH:$vpi_lib"
  fi

  $GHDL --vpi-compile -v gcc -c vpi1.c
  $GHDL --vpi-link -v gcc -o vpi1.vpi vpi1.o

  simulate myentity --vpi=./vpi1.vpi --vpi-trace | tee myentity.out
  if grep -q Error myentity.out; then
      echo "Error in output"
      exit 1;
  fi
  if grep -q error myentity.out; then
      echo "error in output"
      exit 1;
  fi

  rm -f vpi1.vpi vpi1.o
fi
clean

echo "Test successful"
