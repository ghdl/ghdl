#! /bin/sh

. ../../testenv.sh

analyze_failure repro.vhdl

analyze_failure repro2.vhdl

analyze counter.vhdl
analyze_failure tb_counter.vhdl
clean

echo "Test successful"
