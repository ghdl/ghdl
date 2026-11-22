#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze repro.vhdl
elab_simulate repro

analyze gb16.vhdl
elab_simulate gb16

clean

echo "Test successful"
