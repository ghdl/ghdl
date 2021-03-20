#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS='-Wpsl-uncovered --std=08 -fpsl'
analyze top.vhd
elab_simulate tb_top

clean

echo "Test successful"
