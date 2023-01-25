#! /bin/sh

. ../../testenv.sh

analyze test_ip.vhdl
elab_simulate_failure test_ip

analyze st1.vhdl
elab_simulate_failure st1

analyze st2.vhdl
elab_simulate_failure st2

clean

export GHDL_STD_FLAGS=--std=08
analyze st3.vhdl
elab_simulate_failure st3

analyze st4.vhdl
elab_simulate_failure st4

clean

echo "Test successful"
