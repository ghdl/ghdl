#! /bin/sh

. ../../testenv.sh

analyze_failure repro.vhdl

analyze_failure nullrng.vhdl

analyze_failure -Werror nullrng2.vhdl
analyze nullrng2.vhdl

clean

echo "Test successful"
