#! /bin/sh

. ../../testenv.sh

analyze top.vhdl
elab_simulate_failure top_ent

clean

echo "Test successful"
