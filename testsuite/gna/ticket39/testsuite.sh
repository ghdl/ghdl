#! /bin/sh

. ../../testenv.sh

analyze test.vhd
elab_simulate test

clean

echo "Test successful"
