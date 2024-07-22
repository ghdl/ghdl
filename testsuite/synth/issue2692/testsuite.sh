#! /bin/sh

. ../../testenv.sh

synth_failure repro1.vhdl -e
synth_failure repro2.vhdl -e
synth_failure spi_slave.vhdl -e

echo "Test successful"
