#! /bin/sh

. ../../testenv.sh

mkdir -p mylib/v93

analyze --workdir=mylib/v93 --work=mylib mylib.vhdl
analyze mytest.vhdl 
elab_simulate mytest

clean
rm -rf mylib

echo "Test successful"
