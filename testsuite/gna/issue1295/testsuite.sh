#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze psl_next_event_a.vhdl
elab_simulate psl_next_event_a

clean

echo "Test successful"
