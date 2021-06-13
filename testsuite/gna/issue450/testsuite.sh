#! /bin/sh

. ../../testenv.sh

analyze disptree.vhdl
elab disptree

if c_compiler_is_available && ghdl_has_feature disptree vpi; then
  add_vpi_path

  $GHDL --vpi-compile -v gcc -c vpi2.c
  $GHDL --vpi-link -v gcc -o vpi2.vpi vpi2.o

  simulate disptree --vpi=./vpi2.vpi | tee disptree.out
  diff_nocr -q disptree.ref disptree.out

  rm -f vpi2.o vpi2.vpi disptree.out
fi

clean

echo "Test successful"
