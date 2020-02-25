#! /bin/sh

. ../../testenv.sh

analyze tb.vhdl
elab_simulate tb_ghdl_test

analyze count.vhdl
elab count

simulate count < ghdl_test.txt | grep -q " 10"
simulate count < empty.txt | grep -q " 5"

clean

echo "Test successful"
