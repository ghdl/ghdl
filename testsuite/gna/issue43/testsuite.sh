#! /bin/sh

. ../../testenv.sh

analyze repro2.vhdl
elab_simulate test_tb

clean

echo "Test successful"
