#! /bin/sh

. ../../testenv.sh

analyze testcase.vhdl
elab_simulate testcase

clean

echo "Test successful"
