#! /bin/sh

. ../../testenv.sh

analyze --std=93 std_logic_warning.vhdl
#elab_simulate warning_test

analyze test.vhdl
elab_simulate warning_test

clean

echo "Test successful"
