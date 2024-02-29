#! /bin/sh

. ../../testenv.sh

analyze ent.vhdl
elab_simulate ent

clean

echo "Test successful"
