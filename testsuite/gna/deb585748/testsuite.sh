#! /bin/sh

. ../../testenv.sh

analyze 585748_deb.vhd
elab_simulate_failure tb_test

clean

echo "Test successful"
