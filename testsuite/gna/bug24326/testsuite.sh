#! /bin/sh

. ../../testenv.sh

analyze_failure tb_thingy7.vhdl
analyze_failure tb_thingy9.vhdl
analyze_failure repro1.vhdl

clean

echo "Test successful"
