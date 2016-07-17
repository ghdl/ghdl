#! /bin/sh

. ../../testenv.sh

analyze test_load.vhdl
$GHDL --vpi-compile gcc -c vpi1.c
$GHDL --vpi-link gcc -o vpi1.vpi vpi1.o

elab_simulate test_load --vpi=vpi1.vpi

rm -f vpi1.vpi vpi1.o
clean

echo "Test successful"
