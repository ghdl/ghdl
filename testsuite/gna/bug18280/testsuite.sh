#! /bin/sh

. ../../testenv.sh

analyze alias_bug.vhd
elab_simulate alias_bug

clean

echo "Test successful"
