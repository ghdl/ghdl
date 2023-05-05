#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=93
analyze aggr_repro1.vhdl
elab_simulate aggr_repro1

analyze aggr_repro3.vhdl
elab_simulate aggr_repro3

analyze aggr_repro4.vhdl
elab_simulate aggr_repro4

analyze_failure top.vhdl
analyze_failure top3.vhdl

export GHDL_STD_FLAGS=--std=93c
analyze aggr_repro2.vhdl
elab_simulate aggr_repro2

analyze top.vhdl
elab_simulate top

analyze top4.vhdl
elab_simulate top4

clean

echo "Test successful"
