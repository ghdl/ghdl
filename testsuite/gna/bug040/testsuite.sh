#! /bin/sh

. ../../testenv.sh

analyze *.vhd
elab_simulate tb --stop-time=4us

clean

echo "Test successful"
