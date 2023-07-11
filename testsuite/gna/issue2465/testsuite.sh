#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=93
analyze_failure visibility8a.vhdl
analyze_failure visibility8b.vhdl
analyze_failure visibility8c.vhdl
analyze visibility8d.vhdl
analyze_failure visibility8e.vhdl
analyze_failure visibility8f.vhdl
analyze visibility8f2.vhdl
analyze visibility8f3.vhdl
analyze_failure visibility8g.vhdl

clean

echo "Test successful"
