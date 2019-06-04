#! /bin/sh

. ../../testenv.sh

analyze_failure --force-analysis inst1.vhdl

clean

echo "Test successful"
