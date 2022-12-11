#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze repro6.vhdl
elab_simulate MemPkgReproducer6

analyze repro7.vhdl
elab_simulate MemPkgReproducer7

clean

echo "Test successful"
