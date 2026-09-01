#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze repro2.vhdl
elab_simulate repro2

analyze repro3.vhdl
elab_simulate repro3

clean

# The aggregate from the report itself.  Its element strings are 3, 8, 4 and
# 15 characters long, so they cannot all be the element subtype of one array:
# analysis must say which ones do not match, and evaluating it must report the
# bound violation.  Neither may end in an internal error.
check_clean ()
{
  echo "$1"
  if echo "$1" | grep -q "GHDL Bug occurred"; then
    echo "FAIL: crashed instead of reporting a clean diagnostic"
    exit 1
  fi
}

out=$(analyze repro.vhdl 2>&1)
check_clean "$out"
if ! echo "$out" | grep -q "doesn't match aggregate element subtype"; then
  echo "FAIL: expected the element subtype mismatch to be reported"
  exit 1
fi

if out=$(elab_simulate repro 2>&1); then
  echo "$out"
  echo "FAIL: expected the invalid aggregate to be rejected"
  exit 1
fi
check_clean "$out"

clean

# Same aggregate, but as the value of an attribute specification -- which is
# how it appears in the a2i sources this was reported from.  Reading the
# attribute used to report the bound violation and then crash in
# Elab_Attribute_Specification.
out=$(analyze attr.vhdl 2>&1)
check_clean "$out"
if ! echo "$out" | grep -q "doesn't match aggregate element subtype"; then
  echo "FAIL: expected the element subtype mismatch to be reported"
  exit 1
fi

analyze attr_use.vhdl

if out=$(elab_simulate use1772 2>&1); then
  echo "$out"
  echo "FAIL: expected the invalid attribute value to be rejected"
  exit 1
fi
check_clean "$out"
if ! echo "$out" | grep -qE "out of bound expression|bound check failure"; then
  echo "FAIL: expected a bound violation"
  exit 1
fi

clean

echo "Test successful"
