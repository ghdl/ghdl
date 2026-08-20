#! /bin/sh

. ../../testenv.sh

# A conversion function on a port association crashed --synth with an
# internal ASSERT_FAILURE (synth-expr.adb:830).  The maintainer confirmed
# the cause on the thread: "Yes, the conversion function is not handled.
# I will add that."
#
# theunit.vhdl is the report's design, whose actual is an array taken from
# a record field.  rec.vhdl covers the variant a second reporter raised on
# the same thread -- "I get the same error from synth when using
# conversion functions for records" -- where the converted value is itself
# a record.  See issue1502.
synth theunit.vhdl -e theunit > syn_theunit.vhdl
analyze syn_theunit.vhdl

# Only synthesized, not re-analyzed: the netlist still refers to the
# package declaring the record types, which is not part of the netlist.
export GHDL_STD_FLAGS=--std=08
out=$(synth rec.vhdl -e rectop 2>&1)

if echo "$out" | grep -q "GHDL Bug occurred"; then
  echo "FAIL: crashed on a conversion function returning a record"
  exit 1
fi

clean

echo "Test successful"
