#! /bin/sh

. ../../testenv.sh

analyze strings_test.vhdl
elab_simulate strings_test

clean

echo "Test successful"
