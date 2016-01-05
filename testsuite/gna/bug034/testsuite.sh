#! /bin/sh

. ../../testenv.sh

analyze repro.vhdl
elab_simulate ent;

clean

echo "Test successful"
