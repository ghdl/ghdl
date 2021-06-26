#! /bin/sh

. ../../testenv.sh

analyze record_test.vhdl tb_record_test.vhdl
elab_simulate tb_record_test
clean

synth record_test.vhdl -e record_test > syn_record_test.vhdl
analyze syn_record_test.vhdl tb_record_test.vhdl
elab_simulate tb_record_test --ieee-asserts=disable-at-0
clean

echo "Test successful"
