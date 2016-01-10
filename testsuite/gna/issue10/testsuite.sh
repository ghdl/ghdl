#! /bin/sh

. ../../testenv.sh

analyze --std=93 test_id.vhdl
elab_simulate --std=93 test_id
analyze test_id.vhdl
elab_simulate test_id

analyze test_attr.vhdl
elab_simulate test_attr

clean

echo "Test successful"
