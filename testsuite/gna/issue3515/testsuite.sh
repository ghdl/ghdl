#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze gb17.vhdl
elab_simulate gb17

clean

echo "Test successful"
