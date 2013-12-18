#! /bin/sh

. ../../testenv.sh

analyze 18351.vhd
elab_simulate problem

clean

echo "Test successful"
