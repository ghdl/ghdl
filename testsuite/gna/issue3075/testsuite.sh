#! /bin/sh

. ../../testenv.sh

analyze a.vhdl
elab_simulate_failure a

clean

echo "Test successful"
