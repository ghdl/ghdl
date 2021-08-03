#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

analyze repro1.vhdl
elab_simulate repro1

analyze repro2.vhdl
elab_simulate repro2

analyze crash.vhdl crash_tb.vhdl
elab_simulate crash_tb

clean

echo "Test successful"
