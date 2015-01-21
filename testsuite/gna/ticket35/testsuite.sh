#! /bin/sh

. ../../testenv.sh

analyze ascii7.vhdl
analyze latin1.vhdl

analyze_failure utf8.vhdl
analyze_failure utf16be.vhdl
analyze_failure utf16le.vhdl

clean

echo "Test successful"
