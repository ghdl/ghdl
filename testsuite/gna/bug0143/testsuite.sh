#! /bin/sh

. ../../testenv.sh

analyze t.vhdl
elab_simulate_failure t

clean

echo "Test successful"
