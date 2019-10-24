#! /bin/sh

. ../../testenv.sh

analyze types_pkg.vhd
analyze const_pkg.vhd
analyze tester.vhd
analyze generic_check.vhd
analyze tester_conf.vhd
elab_simulate tester_conf

clean

echo "Test successful"
