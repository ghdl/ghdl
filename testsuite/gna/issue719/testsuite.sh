#! /bin/sh

. ../../testenv.sh

analyze tb.vhdl 2> tb.out
ghdl_diff_stcr -q tb.ref tb.out

rm -f tb.out
clean

echo "Test successful"
