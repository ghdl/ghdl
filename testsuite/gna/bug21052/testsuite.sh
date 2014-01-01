#! /bin/sh

. ../../testenv.sh

analyze test.vhd
elab_simulate test --wave=test.ghw

clean

echo "Test successful"
