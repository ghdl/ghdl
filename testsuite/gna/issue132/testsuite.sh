#! /bin/sh

. ../../testenv.sh

analyze_failure repro.vhdl
analyze -C repro.vhdl
analyze --mb-comments repro.vhdl

clean

echo "Test successful"
