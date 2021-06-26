#! /bin/sh

. ../../testenv.sh

if c_compiler_is_available; then
  if [ -z "$CC" ]; then
    CC="gcc"
  fi

  $CC -c -fPIC getrand.c
  $CC -o getrand.so --shared getrand.o

  analyze tb.vhdl
  elab_simulate tb

  rm -f getrand.o getrand.so
fi
clean

echo "Test successful"
