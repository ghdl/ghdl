#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

analyze e4.vhdl
elab_simulate e4

analyze e3.vhdl
elab_simulate e3

analyze e2.vhdl
elab_simulate e2

analyze e1.vhdl
elab_simulate e1

analyze e.vhdl
elab_simulate e

analyze repro1.vhdl
elab_simulate repro1

clean

echo "Test successful"
