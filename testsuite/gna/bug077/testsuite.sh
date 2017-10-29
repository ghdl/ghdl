#! /bin/sh

. ../../testenv.sh

analyze repro2.vhdl
elab_simulate repro2

analyze repro5.vhdl
elab_simulate repro5

analyze repro6.vhdl
elab_simulate_failure repro6

clean

export GHDL_STD_FLAGS=--std=08
analyze repro.vhdl
elab_simulate repro

analyze repro3.vhdl
elab_simulate repro3

analyze repro7.vhdl
elab_simulate repro7

clean

echo "Test successful"
