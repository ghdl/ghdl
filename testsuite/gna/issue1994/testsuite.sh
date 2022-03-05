#! /bin/sh

. ../../testenv.sh

analyze tb.vhdl
elab_simulate tb

clean

echo "Test successful"
