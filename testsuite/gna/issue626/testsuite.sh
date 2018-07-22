#! /bin/sh

. ../../testenv.sh

analyze repro.vhdl
elab_simulate repro
analyze top.vhdl
elab_simulate top_ent

clean

echo "Test successful"
