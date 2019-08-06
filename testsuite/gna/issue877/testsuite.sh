#! /bin/sh

. ../../testenv.sh

analyze aa2.vhdl

analyze tb.vhdl
elab_simulate tb

clean

echo "Test successful"
