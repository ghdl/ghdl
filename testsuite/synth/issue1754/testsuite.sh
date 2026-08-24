#! /bin/sh

. ../../testenv.sh

# A record field typed as an array of unconstrained std_logic_vector
# elements (a generic array of vectors pattern) used to crash --synth
# with an internal CONSTRAINT_ERROR (synth-vhdl_context.adb:234) even
# though the declaration was never used. See issue1754.
export GHDL_STD_FLAGS="--std=08"
synth my_entity.vhd -e my_entity > syn_my_entity.vhd
analyze syn_my_entity.vhd

clean

echo "Test successful"
