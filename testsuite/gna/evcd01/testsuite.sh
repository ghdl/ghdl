#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

analyze dut.vhdl
analyze tb.vhdl
elab tb

if ghdl_has_feature tb evcd; then
  simulate tb --evcd-nodate --evcd=tb.evcd

  # Ports are declared with the extended-VCD "port" var type, scalars as
  # width 1 and vectors with their index range.
  if ! grep -q '^.var port 1 <0 clk .end' tb.evcd; then
    echo "missing scalar port declaration in evcd"
    exit 1
  fi
  if ! grep -q '^.var port \[3:0\] <2 data_in .end' tb.evcd; then
    echo "missing vector port declaration in evcd"
    exit 1
  fi

  # The initial values are wrapped in a dumpports block ...
  if ! grep -q '^.dumpports' tb.evcd; then
    echo "missing dumpports block in evcd"
    exit 1
  fi

  # ... and emitted as 'p' records carrying per-bit state plus the two
  # drive-strength fields.  data_in = "0001" -> states DDDU (input low/high),
  # strength0 6660, strength1 0006.
  if ! grep -q '^pDDDU 6660 0006 <2$' tb.evcd; then
    echo "wrong extended-VCD value record for input vector"
    exit 1
  fi

  # The inout port is high-Z during reset -> three-state, strengths 0 0.
  if ! grep -q '^pF 0 0 <5$' tb.evcd; then
    echo "missing inout three-state record in evcd"
    exit 1
  fi

  # The closing simulation time is recorded.
  if ! grep -q '^.vcdclose ' tb.evcd; then
    echo "missing vcdclose marker in evcd"
    exit 1
  fi
fi

clean
rm -f tb.evcd

echo "Test successful"
