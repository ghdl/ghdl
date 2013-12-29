#! /bin/sh

. ../../testenv.sh

analyze 15368.vhd
elab_simulate bug

clean

echo "Test successful"
