#! /bin/sh

. ../../testenv.sh

analyze repro1.vhdl
elab_simulate repro1

analyze ent.vhdl

clean

echo "Test successful"
