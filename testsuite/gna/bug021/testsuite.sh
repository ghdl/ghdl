#! /bin/sh

. ../../testenv.sh

analyze sim_pkg.vhd
analyze_failure tb_cosim.vhd

clean

echo "Test successful"
