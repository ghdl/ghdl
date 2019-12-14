#! /bin/sh

. ../../testenv.sh

analyze ent.vhdl
elab_simulate ent

analyze repro.vhdl
elab_simulate repro

clean

echo "Test successful"
