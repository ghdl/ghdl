#!/bin/sh

. ../../testenv.sh

analyze test_pkg.vhdl test.vhdl tb_test.vhdl
elab_simulate tb_test --assert-level=error
clean

synth test_pkg.vhdl test.vhdl -e test > syn_test.vhdl
analyze test_pkg.vhdl syn_test.vhdl tb_test.vhdl
elab_simulate tb_test --ieee-asserts=disable-at-0 --assert-level=error
clean

synth test_pkg.vhdl test2.vhdl -e test2 > syn_test2.vhdl

echo "Test successful"
