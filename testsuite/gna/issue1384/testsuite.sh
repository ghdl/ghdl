#! /bin/sh

. ../../testenv.sh

analyze -O3 tb_issue.vhdl

clean

echo "Test successful"
