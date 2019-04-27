#! /bin/sh

. ../../testenv.sh

gcc -c -fPIC getrand.c
gcc -o getrand.so --shared getrand.o

analyze tb.vhdl
elab_simulate tb

clean
rm -f getrand.o getrand.so

echo "Test successful"
