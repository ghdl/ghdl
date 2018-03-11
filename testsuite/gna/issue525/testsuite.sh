#! /bin/sh

. ../../testenv.sh

analyze_failure counter_illegal_attribute.vhd

# By default, emit only a warning when trying to specify an attribute for a
# port from the architecture.
analyze repro1.vhdl

# Or in relaxed mode
export GHDL_STD_FLAGS="--std=93 -frelaxed-rules"
analyze repro1.vhdl

# But it is an error in strict mode
export GHDL_STD_FLAGS=--std=93
analyze_failure repro1.vhdl

clean

echo "Test successful"
