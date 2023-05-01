#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=93
analyze aggr_repro1.vhdl
elab_simulate aggr_repro1

analyze aggr_repro3.vhdl
elab_simulate aggr_repro3

analyze aggr_repro4.vhdl
elab_simulate aggr_repro4

export GHDL_STD_FLAGS=--std=93c
analyze aggr_repro2.vhdl
elab_simulate aggr_repro2

clean

#export GHDL_STD_FLAGS=--std=08
#analyze repro.vhdl
#elab_simulate repro

#clean

echo "Test successful"
