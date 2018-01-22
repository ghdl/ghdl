#! /bin/sh

. ../../testenv.sh

# Check that it is possible to override ieee library
analyze --work=ieee pkg.vhdl

analyze tieee.vhdl
elab_simulate tieee

analyze_failure tieee2.vhdl

clean
clean ieee

echo "Test successful"
