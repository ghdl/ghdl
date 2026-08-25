#! /bin/sh

. ../../testenv.sh

# A function reading a signal from its enclosing architecture, called from
# a clocked process, crashed --synth with an internal ASSERT_FAILURE
# (synth-stmts.adb:2055).  The report attributed it to the case statement
# having more than one return statement, but the maintainer identified the
# actual cause on the thread: it is the function being impure -- "It is not
# the number of return statements but the fact the function is impure.
# Impure functions are not well handled."
#
# bug.vhdl is the report's design.  bug_pure.vhdl is the follow-up from the
# thread ("Still crashes if it's marked as pure instead of impure"), which
# is the same function declared pure while still reading the signal; GHDL
# accepts it with a -Wpure warning.  See issue1574.
synth bug.vhdl -e bug > syn_bug.vhdl
analyze syn_bug.vhdl

synth bug_pure.vhdl -e bug_pure > syn_bug_pure.vhdl
analyze syn_bug_pure.vhdl

clean

echo "Test successful"
