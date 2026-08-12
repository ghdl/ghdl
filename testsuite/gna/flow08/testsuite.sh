#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

analyze dut.vhdl tb.vhdl
elab tb

#  --flow=FILE writes the native dataflow database (.flow JSON).
#  The dump happens at elaboration, so a zero-time run is enough.
simulate tb --flow=flow08.flow --stop-time=0ns

if test ! -s flow08.flow; then
  echo "FAILED: --flow did not create flow08.flow"
  exit 1
fi

check ()
{
  if ! grep -qF "$1" flow08.flow; then
    echo "FAILED: expected $2 in flow08.flow"
    exit 1
  fi
}

#  Lexical-position helpers for source-scanning consumers (e.g. Perl
#  lexers).  Every position is the compact byte-offset string
#  "start:begin:end" (begin/end empty when not applicable).

#  Entity and architecture are separate units, each with its own span.
#  dut entity: "entity dut is" .. "end dut;" (no begin).
check '"name": "dut", "file": "dut.vhdl", "pos": "180::433"' "entity span"

#  dut architecture: "architecture" : 'begin' : final "end;".
check '"name": "rtl", "entity": "dut", "file": "dut.vhdl", "pos": "443:527:911"' "architecture start:begin:end"

#  Per-unit context linkage: library/use clauses with positions, attached
#  to the entity (the clauses precede `entity dut`).
check '"libraries": ["ieee"]'                          "library clause list"
check '"name": "ieee.numeric_std.all", "pos": "43::"'  "use clause linkage to package"

#  Real process: start : 'begin' (end of declarations) : "end process;".
check '"label": "P0", "pos": "535:559:703"'            "process start:begin:end"

#  Concurrent assignment becomes a process with no begin: start :: ';'.
check '"label": "P1", "pos": "719::750"'               "concurrent assignment span"

#  Entity instantiation: start :: terminating ';' (no begin).
check '"label": "uut", "entity": "dut", "is_entity_inst": true, "pos": "344::486"' "instantiation span"

clean
rm -f flow08.flow

echo "Test successful"
