#! /bin/sh

. ../../testenv.sh

# A function call producing a record-field-derived array actual for a
# port association used to crash --synth with an internal ASSERT_FAILURE
# (synth-expr.adb:830). See issue1502.
synth theunit.vhdl -e theunit > syn_theunit.vhdl
analyze syn_theunit.vhdl

clean

echo "Test successful"
