#! /bin/sh

. ../../testenv.sh

analyze test.vhdl
elab_simulate test

clean

echo "Test successful"
