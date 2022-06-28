#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze ent.vhdl
elab_simulate ent > ent.out

if grep TRUE ent.out; then
  exit 1
fi

if grep FALSE ent.out; then
  exit 1
fi

clean

echo "Test successful"
