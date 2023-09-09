#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze ent.vhdl
elab_simulate ent

analyze -Werror --expect-failure ent.vhdl

clean

echo "Test successful"
