#! /bin/sh

. ../../testenv.sh

synth_failure -fsynopsys uart_rx.vhdl -e

echo "Test successful"
