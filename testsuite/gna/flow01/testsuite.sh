#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

analyze dut.vhdl tb.vhdl
elab tb

#  --flow=FILE writes the native dataflow database (.flow JSON).
#  The dump happens at elaboration, so a zero-time run is enough.
simulate tb --flow=flow01.flow --stop-time=0ns

#  The file must exist and be non-empty.
if test ! -s flow01.flow; then
  echo "FAILED: --flow did not create flow01.flow"
  exit 1
fi

check ()
{
  if ! grep -qF "$1" flow01.flow; then
    echo "FAILED: expected $2 in flow01.flow"
    exit 1
  fi
}

#  Top-level provenance and elaborated top unit.
check '"schema": "flowtracer1.vhdl.v0"' "vhdl schema"
check '"top": "tb"'                        "elaborated top unit"

#  architectures[]: process dataflow (read-set / drive-set) from the AST.
check '"drives": ["cnt"]'                  "clocked process drive set"

#  hierarchy[]: elaborated instance tree with the real port_map.
check '"instance_label": "uut"'            "elaborated instance"
check '"actuals": ["clk"]'                 "real port binding"

#  nets[] / cells[]: canonical net graph + comb-vs-clocked.
#  The dut clk port collapses into the tb clk net (port<->net unification).
check '"clocked": true'                    "clock classification"
check '"aliases": ["clk"]'                 "signal collapsing (port<->net)"

#  Bare --flow defaults to ghdl.flow.
simulate tb --flow --stop-time=0ns
if test ! -s ghdl.flow; then
  echo "FAILED: bare --flow did not create ghdl.flow"
  exit 1
fi

clean
rm -f flow01.flow ghdl.flow

echo "Test successful"
