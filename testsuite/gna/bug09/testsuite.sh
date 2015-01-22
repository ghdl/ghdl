#! /bin/sh

. ../../testenv.sh

analyze univ1.vhdl
elab_simulate_failure univ1

analyze univ2.vhdl
elab_simulate univ2

clean

echo "Test successful"
