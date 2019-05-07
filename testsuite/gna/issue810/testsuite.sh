#! /bin/sh

. ../../testenv.sh

analyze repro.vhdl
elab_simulate repro

analyze nullrng.vhdl
analyze_failure -Werror nullrng.vhdl

clean

echo "Test successful"
