#! /bin/sh

. ../../testenv.sh

analyze testent.vhdl
elab_simulate test_core

clean

echo "Test successful"
