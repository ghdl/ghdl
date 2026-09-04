#! /bin/sh

. ../../testenv.sh

# Reduced from Debug.zip attached to the report.  A case choice that is a
# constant whose value comes from a function call is not locally static, so
# it must be rejected -- GHDL 0.35 raised
#   build_constant: cannot handle IIR_KIND_AGGREGATE
# and an internal error instead.

export GHDL_STD_FLAGS="--std=08 --ieee=synopsys"

if out=$(analyze repro.vhd 2>&1); then
  echo "$out"
  echo "FAIL: expected the non-static choice to be rejected"
  exit 1
fi
echo "$out"

if echo "$out" | grep -q "GHDL Bug occurred"; then
  echo "FAIL: crashed instead of reporting a clean error"
  exit 1
fi

if ! echo "$out" | grep -q "choice must be locally static expression"; then
  echo "FAIL: expected the locally-static diagnostic"
  exit 1
fi

clean

echo "Test successful"
