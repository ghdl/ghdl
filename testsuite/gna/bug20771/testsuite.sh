#! /bin/sh

. ../../testenv.sh

analyze 20771.vhd
elab_simulate_failure jon

clean

echo "Test successful"
