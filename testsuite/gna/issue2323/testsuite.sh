#! /bin/sh

. ../../testenv.sh

analyze test_ip.vhdl
elab_simulate_failure test_ip

analyze st1.vhdl
elab_simulate_failure st1

analyze st2.vhdl
elab_simulate_failure st2

clean

echo "Test successful"
