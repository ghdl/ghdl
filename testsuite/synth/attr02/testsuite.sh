#! /bin/sh

. ../../testenv.sh

for t in succ01 pred01 leftof01 rightof01 left01; do
    synth_tb $t
done

synth_failure err_last_val.vhdl -e
synth_failure err_last_active.vhdl -e
synth_failure err_last_event.vhdl -e
synth_failure err_driving_value.vhdl -e
synth_failure err_driving.vhdl -e

GHDL_STD_FLAGS=--std=08
synth_tb right01

echo "Test successful"
