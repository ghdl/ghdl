#! /bin/sh

. ../../testenv.sh

analyze_failure enum01.vhdl 2> ana.err

grep -q "s1 to s2" ana.err

clean

echo "Test successful"
