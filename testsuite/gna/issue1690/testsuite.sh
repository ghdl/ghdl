#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS='--std=08 -fpsl'
analyze top.vhd
elab_simulate tb_top --psl-report-uncovered

clean

echo "Test successful"
