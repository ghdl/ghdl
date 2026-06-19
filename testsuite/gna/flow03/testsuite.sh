#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

analyze genr.vhdl
elab gtb

#  A for-generate must be unrolled in the elaborated hierarchy, with the
#  real loop value as the generate index.
simulate gtb --flow=flow03.flow --stop-time=0ns

if test ! -s flow03.flow; then
  echo "FAILED: --flow did not create flow03.flow"
  exit 1
fi

check ()
{
  if ! grep -qF "$1" flow03.flow; then
    echo "FAILED: expected $2 in flow03.flow"
    exit 1
  fi
}

#  Generate frames unrolled with the actual VHDL loop values (0..2),
#  marked as transparent scopes.
check '"generate": {"kind": "for", "label": "g", "index": 0}' "generate index 0"
check '"generate": {"kind": "for", "label": "g", "index": 1}' "generate index 1"
check '"generate": {"kind": "for", "label": "g", "index": 2}' "generate index 2"
check '"scope": true' "transparent generate scope"

#  Each unrolled iteration instantiates the cell entity.
check '"module": "cell"' "generate-internal instance"

clean
rm -f flow03.flow

echo "Test successful"
