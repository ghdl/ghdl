#! /bin/sh

. ../../testenv.sh

analyze ent.vhdl
elab_simulate board

clean

echo "Test successful"
