#! /bin/sh

. ../../testenv.sh

analyze repro1.vhdl
elab sliced_ex

if ghdl_has_feature sliced_ex vpi; then
  $GHDL --vpi-compile -v gcc -c vpi1.c
  $GHDL --vpi-link -v gcc -o vpi1.vpi vpi1.o

  if [ "$OS" = "Windows_NT" ]; then
      vpi_lib=`$GHDL --vpi-library-dir | sed -e 's!\\\\!/!g' -e 's!^C:!/C!g'`
      echo vpi_lib: $vpi_lib
      PATH="$PATH:$vpi_lib"
  fi

  simulate sliced_ex --vpi=./vpi1.vpi | tee sliced_ex.out
  if grep -q Error sliced_ex.out; then
      echo "Error in output"
      exit 1;
  fi

  rm -f vpi1.vpi vpi1.o sliced_ex.out
fi
clean

echo "Test successful"
