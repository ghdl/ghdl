#! /bin/sh

. ../../testenv.sh

analyze bar.vhdl
elab_failure bar

analyze_failure bar0.vhdl

analyze_failure bar2.vhdl
analyze_failure bar3.vhdl

analyze bar4.vhdl

clean

echo "Test successful"
