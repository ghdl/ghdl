#! /bin/sh

. ../../testenv.sh

analyze  test.vhdl
elab_simulate test_val

clean

echo "Test successful"
