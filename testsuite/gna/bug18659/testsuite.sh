#! /bin/sh

. ../../testenv.sh

analyze crash.vhd
elab_simulate crash

clean

echo "Test successful"
