#! /bin/sh

. ../../testenv.sh

analyze 7751_tests.vhd
elab_simulate top

clean

echo "Test successful"
