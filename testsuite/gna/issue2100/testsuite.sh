#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze_failure ent.vhdl 2> log.err
if grep 'no overloaded function' log.err; then
  exit 1
fi

clean

echo "Test successful"
