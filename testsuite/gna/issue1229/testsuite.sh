#! /bin/sh

. ../../testenv.sh

# gcc -c -fPIC aux.c
# gcc -shared -o aux.so aux.o

export GHDL_STD_FLAGS=--std=08
analyze pkg.vhdl tb.vhdl
# elab_simulate tb

clean
# rm -f aux.o aux.so

echo "Test successful"
