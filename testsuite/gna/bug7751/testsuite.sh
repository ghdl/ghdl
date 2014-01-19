#! /bin/sh

. ../../testenv.sh

analyze 7751_tests.vhd
elab_simulate top

analyze 7751_extra_tests.vhd
elab_simulate tb

clean

echo "Test successful"
