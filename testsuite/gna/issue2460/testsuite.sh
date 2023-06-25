#! /bin/sh

. ../../testenv.sh

# When analyzing a unit, dependencies must have been analyzed 
$GHDL -i test_ghdl.vhdl
elab_failure test_ghdl

clean

echo "Test successful"
