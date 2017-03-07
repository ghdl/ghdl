#! /bin/sh

. ../../testenv.sh

analyze test_bench.vhdl
elab_simulate test_bench --stop-time=10ns

# To check: generate .ghw, check for no 'U'

clean

echo "Test successful"
