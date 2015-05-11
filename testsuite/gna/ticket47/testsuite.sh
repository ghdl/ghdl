#! /bin/sh

. ../../testenv.sh

analyze ent.vhdl
elab_simulate ent -gstr=Hello
elab_simulate_failure ent

clean

echo "Test successful"
