#! /bin/sh

. ../../testenv.sh

analyze disptree.vhdl
elab disptree

if ghdl_has_feature disptree vpi; then
  if [ "$OS" = "Windows_NT" ]; then
      vpi_lib=`$GHDL --vpi-library-dir | sed -e 's!\\\\!/!g' -e 's!^C:!/C!g'`
      echo vpi_lib: $vpi_lib
      PATH="$PATH:$vpi_lib"
  fi

  $GHDL --vpi-compile -v gcc -c vpi2.c
  $GHDL --vpi-link -v gcc -o vpi2.vpi vpi2.o

  simulate disptree --vpi=./vpi2.vpi | tee disptree.out
  diff --strip-trailing-cr -q disptree.ref disptree.out

  rm -f vpi2.o vpi2.vpi disptree.out
fi

clean

echo "Test successful"
