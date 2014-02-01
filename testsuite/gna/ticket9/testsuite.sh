#! /bin/sh

. ../../testenv.sh

analyze index_range_test_A.vhd
elab_simulate_failure index_range_test_A

analyze index_range_test_B.vhd
elab_simulate_failure index_range_test_B

clean

echo "Test successful"
