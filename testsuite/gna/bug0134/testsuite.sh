#! /bin/sh

. ../../testenv.sh

analyze test_logic.vhdl
elab_simulate test_logic

clean

echo "Test successful"
