#! /bin/sh

. ../../testenv.sh

# A process with two separate "if rising_edge(clk) ..." statements
# (rather than one enclosing if with nested logic) used to crash --synth
# with an internal ASSERT_FAILURE (netlists.adb:529). See issue1235.
export GHDL_STD_FLAGS="--std=08"
synth ent.vhd -e ent > syn_ent.vhd
analyze syn_ent.vhd

clean

echo "Test successful"
