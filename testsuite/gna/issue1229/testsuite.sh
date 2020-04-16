#! /bin/sh

. ../../testenv.sh

# gcc -c -fPIC caux.c
# gcc -shared -o caux.so caux.o

export GHDL_STD_FLAGS=--std=08
analyze pkg.vhdl tb.vhdl
# elab_simulate tb

clean
# rm -f caux.o caux.so

echo "Test successful"
