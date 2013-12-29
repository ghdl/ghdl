#! /bin/sh

. ../../testenv.sh

analyze bug.vhdl
elab_simulate bug

clean

echo "Test successful"
