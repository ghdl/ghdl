#! /bin/sh

. ../../testenv.sh

analyze tb.vhdl
analyze tb2.vhdl

clean

echo "Test successful"
