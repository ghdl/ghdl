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

synth_only leftof02
analyze leftof02.vhdl
elab_simulate leftof02
clean

synth_only rightof02
analyze rightof02.vhdl
elab_simulate rightof02
clean

synth_only name01
synth_only name02

synth_failure leftof03.vhdl -e

GHDL_STD_FLAGS=--std=08
synth_tb right01

echo "Test successful"
