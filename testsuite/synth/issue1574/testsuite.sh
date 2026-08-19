#! /bin/sh

. ../../testenv.sh

# A function with a case statement containing more than one return
# statement used to crash --synth with an internal ASSERT_FAILURE
# (synth-stmts.adb:2055). See issue1574.
synth bug.vhdl -e bug > syn_bug.vhdl
analyze syn_bug.vhdl

clean

echo "Test successful"
