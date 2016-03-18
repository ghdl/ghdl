#! /bin/sh

. ../../testenv.sh

analyze loopfilter.vhdl
elab_simulate loopfilter

clean

echo "Test successful"
