#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze simple_fsm.vhdl tb_simple_fsm.vhdl
elab_simulate tb_simple_fsm

analyze repro.vhdl
elab_simulate repro

clean

echo "Test successful"
