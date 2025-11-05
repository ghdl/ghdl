#! /bin/sh

. ../../testenv.sh

analyze test2.vhdl
elab_simulate tb_test2

clean

echo "Test successful"
