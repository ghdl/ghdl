#! /bin/sh

. ../../testenv.sh

analyze test.vhdl
elab_simulate a

clean

echo "Test successful"
