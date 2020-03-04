#! /bin/sh

. ../../testenv.sh

analyze_failure hello.vhdl -fno-color-diagnostics 2>&1 | grep "no option expected"

clean

echo "Test successful"
