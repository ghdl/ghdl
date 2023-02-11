#! /bin/sh

. ../../testenv.sh

# Normal flow
analyze a.vhdl
analyze b.vhdl

clean

# When analyzing a unit, dependencies must have been analyzed 
$GHDL -i a.vhdl
analyze_failure b.vhdl

clean

# Original test
$GHDL -i b.vhdl c.vhdl a.vhdl
$GHDL -s c.vhdl

clean

echo "Test successful"
