#! /bin/sh

. ../../testenv.sh

analyze_failure --std=93 repro.vhdl
analyze --std=93 -C repro.vhdl
analyze --std=93 --mb-comments repro.vhdl

clean

echo "Test successful"
