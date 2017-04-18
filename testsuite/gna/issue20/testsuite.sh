#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze fp_write_read_issue.vhdl
elab_simulate fp_write_read_issue

rm -f fp_write_read_file_A.txt
clean

echo "Test successful"
