#! /bin/sh

. ../../testenv.sh

analyze 585750_deb.vhd
elab_simulate_failure tb_test

clean

echo "Test successful"
