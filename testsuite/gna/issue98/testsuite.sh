#! /bin/sh

#echo "Test skipped"
#exit 0

. ../../testenv.sh

if [ "$OS" = "Windows_NT" ]; then
    vpi_lib=`$GHDL --vpi-library-dir | sed -e 's!\\!/!g' -e 's!^C:!/C!g'`
    echo vpi_lib: $vpi_lib
    PATH="$PATH:$vpi_lib"
fi

analyze test_load.vhdl
$GHDL --vpi-compile -v gcc -c vpi1.c
$GHDL --vpi-link -v gcc -o vpi1.vpi vpi1.o

elab_simulate test_load --vpi=./vpi1.vpi

rm -f vpi1.vpi vpi1.o
clean

echo "Test successful"
