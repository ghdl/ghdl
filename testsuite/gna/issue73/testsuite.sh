#! /bin/sh

. ../../testenv.sh

analyze x.vhdl
elab_simulate test

clean

echo "Test successful"
