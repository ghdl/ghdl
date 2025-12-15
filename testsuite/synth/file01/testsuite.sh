#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth_failure file02.vhdl -e

synth gen_file_ints.vhdl -e
synth gen_file_vec.vhdl -e

synth_failure file03.vhdl -e
synth_failure file04.vhdl -e
synth_failure file05.vhdl -e
synth_only file06
synth_failure file07.vhdl -e
synth_only file08
synth_only file09
synth_failure file10.vhdl -e
synth_failure file11.vhdl -e
synth_failure file12.vhdl -e
synth_failure file13.vhdl -e

echo "Test successful"
