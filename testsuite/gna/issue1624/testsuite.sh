#! /bin/sh

. ../../testenv.sh

analyze buggy.vhdl
elab_simulate buggy

clean

echo "Test successful"
