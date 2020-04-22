#! /bin/sh

. ../../testenv.sh

analyze_failure -Werror=runtime-error vc.vhdl

clean

echo "Test successful"
