#! /bin/sh

. ../../testenv.sh

analyze ent.vhdl
elab_simulate anotherbug

clean

echo "Test successful"
