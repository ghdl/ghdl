#! /bin/sh

. ../../testenv.sh

if [ -z $CC ]; then
  CC="gcc"
fi

$CC -c -fPIC getrand.c
$CC -o getrand.so --shared getrand.o

analyze tb.vhdl
elab_simulate tb

clean
rm -f getrand.o getrand.so

echo "Test successful"
