#! /bin/sh

. ../../testenv.sh

analyze bug.vhdl
elab_simulate foo
clean

echo "Test successful"
