#! /bin/sh

. ../../testenv.sh

analyze test.vhdl
elab_simulate test

analyze test2.vhdl
elab_simulate test2

clean

echo "Test successful"
