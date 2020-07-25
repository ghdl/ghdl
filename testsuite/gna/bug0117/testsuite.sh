#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze repro1.vhdl
elab_simulate repro1

analyze repro4.vhdl
elab_simulate repro4

analyze repro5.vhdl
elab_simulate repro5

analyze repro7.vhdl
elab_simulate repro7

clean

echo "Test successful"
