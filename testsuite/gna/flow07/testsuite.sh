#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

analyze dut.vhdl tb.vhdl

#  The dedicated `ghdl --flow` command elaborates UNIT and dumps the
#  dataflow database, without running a simulation.
"$GHDL" --flow --std=08 --out=cmd.flow tb
if test ! -s cmd.flow; then
  echo "FAILED: ghdl --flow --out= did not create cmd.flow"
  exit 1
fi
if ! grep -qF '"top": "tb"' cmd.flow; then
  echo "FAILED: cmd.flow missing elaborated top"
  exit 1
fi
if ! grep -qF '"instance_label": "uut"' cmd.flow; then
  echo "FAILED: cmd.flow missing elaborated hierarchy"
  exit 1
fi

#  Bare --flow defaults to ghdl.flow.
"$GHDL" --flow --std=08 tb
if test ! -s ghdl.flow; then
  echo "FAILED: bare ghdl --flow did not create ghdl.flow"
  exit 1
fi

clean
rm -f cmd.flow ghdl.flow

echo "Test successful"
