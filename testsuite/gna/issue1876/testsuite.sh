#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze ent.vhdl
elab_simulate ent -gmygeneric="Hello"
analyze pkgent.vhdl
elab_simulate pkgent -gmygeneric="Hello"

clean

echo "Test successful"
