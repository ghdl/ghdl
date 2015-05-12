#! /bin/sh

. ../../testenv.sh

analyze bug.vhdl
elab_simulate ent

clean

echo "Test successful"
