#! /bin/sh

. ../../testenv.sh

# Test that VCD only emits timestamps when filtered signals change,
# and emits a final timestamp at simulation end.

analyze tb.vhdl
elab tb

if ghdl_has_feature tb vcd; then
  simulate tb --vcd=out.vcd --read-wave-opt=filter.signals --stop-time=30ns

  ts_count=$(grep -c "^#" out.vcd)

  # Expected: #0, #5000000 (low_freq goes 1->0), #30000000 (final)
  # That's 3 timestamps total.
  # Before the fix, there were 7 timestamps (one for each 5ns cycle).
  if [ "$ts_count" -ne 3 ]; then
    echo "error: expected 3 timestamps, got $ts_count"
    cat out.vcd
    exit 1
  fi

  if ! grep -q "^#0$" out.vcd; then
    echo "error: missing initial timestamp #0"
    exit 1
  fi

  if ! grep -q "^#5000000$" out.vcd; then
    echo "error: missing timestamp #5000000 for signal change"
    exit 1
  fi

  if ! grep -q "^#30000000$" out.vcd; then
    echo "error: missing final timestamp #30000000"
    exit 1
  fi

  rm -f out.vcd
  clean
fi

echo "Test successful"
