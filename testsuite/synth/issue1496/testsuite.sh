#! /bin/sh

. ../../testenv.sh

# A target aggregate combining a scalar carry-out with a generic-sized
# vector sum ((cout, sum) <= std_logic_vector(unsigned('0' & a) + ...))
# used to crash --synth with an internal TYPES.INTERNAL_ERROR
# (synth-objtypes.adb:365). See issue1496.
export GHDL_STD_FLAGS="--std=08"
synth -gN=4 adder.vhd -e adder > syn_adder.vhd
analyze syn_adder.vhd

clean

echo "Test successful"
