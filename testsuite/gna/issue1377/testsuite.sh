#! /bin/sh

. ../../testenv.sh

analyze -O tb_issue.vhdl
analyze -O tb_issue2.vhdl

clean

echo "Test successful"
