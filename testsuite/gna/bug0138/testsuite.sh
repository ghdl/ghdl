#! /bin/sh

. ../../testenv.sh

analyze_failure repro.vhdl 2> log.err
grep -q repro.vhdl:13:6 log.err

clean

echo "Test successful"
