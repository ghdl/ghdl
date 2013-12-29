#! /bin/sh

. ../../testenv.sh

analyze 16287.vhd
elab_simulate test

clean

echo "Test successful"
