#! /bin/sh

. ../../testenv.sh

analyze junk2.vhdl
analyze_failure --std=93 junk2.vhdl
analyze --std=02 -frelaxed-rules junk2.vhdl
clean

echo "Test successful"
