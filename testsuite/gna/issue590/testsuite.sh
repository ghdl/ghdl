#! /bin/sh

. ../../testenv.sh

# Reduced from FailLinApprox.zip attached to the report: a procedure call that
# associates one element of an array formal, "Data(0) => InData", where the
# array's element subtype is unbounded.
#
# Translating it hit "pragma Assert (Get_Kind (Formal) =
# Iir_Kind_Selected_Element)" -- the FIXME right above it says only records
# are supported -- and GHDL died with an internal error.  It has to say so
# instead.  Where the message appears depends on the backend: gcc and llvm
# translate while analysing, the pre-elaborated backends while elaborating.

export GHDL_STD_FLAGS=--std=08

if ghdl_is_preelaboration; then
  analyze repro.vhd
  out=$(elab_simulate repro 2>&1) && status=0 || status=1
else
  out=$(analyze repro.vhd 2>&1) && status=0 || status=1
fi
echo "$out"

if [ "$status" -eq 0 ]; then
  echo "FAIL: expected the unsupported association to be rejected"
  exit 1
fi

if echo "$out" | grep -q "GHDL Bug occurred"; then
  echo "FAIL: crashed instead of reporting a clean error"
  exit 1
fi

if ! echo "$out" | grep -q "individual association of an element of an array"; then
  echo "FAIL: expected the not-supported diagnostic"
  exit 1
fi

clean

echo "Test successful"
