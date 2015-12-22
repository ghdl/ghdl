#! /bin/sh

. ../../testenv.sh

analyze_failure junk1.vhd
analyze --std=08 junk1.vhd
analyze junk1ok.vhd
clean
clean --std=08

echo "Test successful"
