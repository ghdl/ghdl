#! /bin/sh

. ../../testenv.sh

analyze file1.vhdl file2.vhdl
elab_simulate file2

clean

echo "Test successful"
