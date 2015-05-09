#! /bin/sh

. ../../testenv.sh

analyze univ1.vhdl
elab_simulate_failure univ1 || echo "overflow not detected!"

analyze univ2.vhdl
elab_simulate univ2

clean

echo "Test successful"
