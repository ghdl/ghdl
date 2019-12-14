#! /bin/sh

. ../../testenv.sh

analyze ent1.vhdl
elab_simulate ent

clean

analyze ent2.vhdl

clean

echo "Test successful"
