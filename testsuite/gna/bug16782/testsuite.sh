#! /bin/sh

. ../../testenv.sh

analyze bug.vhd
elab_simulate_failure bug

clean

echo "Test successful"
