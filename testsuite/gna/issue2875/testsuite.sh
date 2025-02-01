#! /bin/sh

. ../../testenv.sh

analyze repro.vhdl
elab_simulate repro

analyze ent.vhdl

clean

echo "Test successful"
