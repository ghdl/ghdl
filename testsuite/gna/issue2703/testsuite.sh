#! /bin/sh

. ../../testenv.sh

# A leading keyword typo ("packacke") makes the parser bail out through its
# error-recovery path in Parse_Design_Unit, which used to leave a design
# unit with a Null_Iir Library_Unit in the design file. `file-to-xml` (and
# only that command) then registered that malformed unit into the library
# without checking for parse errors first, crashing with an internal
# ASSERTION_ERROR instead of just reporting the parse error. See issue2703.
echo "file-to-xml (failure expected) ComplexEntity.vhdl"
if OUT=$("$GHDL" file-to-xml ComplexEntity.vhdl 2>&1); then
  echo "$OUT"
  echo "FAIL: expected file-to-xml to fail on malformed input"
  exit 1
fi
echo "$OUT"

if echo "$OUT" | grep -q "GHDL Bug occurred"; then
  echo "FAIL: file-to-xml crashed instead of reporting a clean parse error"
  exit 1
fi

if ! echo "$OUT" | grep -q "missing entity, architecture, package or configuration"; then
  echo "FAIL: expected clean parse error message not found"
  exit 1
fi

clean

echo "Test successful"
