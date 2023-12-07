#! /bin/sh

. ../../testenv.sh

analyze_failure repro1.vhdl 2> repro.err
grep -q 'actual port "s"' repro.err

clean

echo "Test successful"
