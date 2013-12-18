#! /bin/sh

. ../../testenv.sh

analyze test.vhd
elab_simulate e

analyze test_20255.vhd
elab_simulate e

clean

echo "Test successful"
