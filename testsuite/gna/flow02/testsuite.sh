#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

analyze gen.vhdl
elab gentb

#  Two instances of the same entity with different generics; the .flow
#  hierarchy must carry the per-instance ELABORATED generic values.
simulate gentb --flow=flow02.flow --stop-time=0ns

if test ! -s flow02.flow; then
  echo "FAILED: --flow did not create flow02.flow"
  exit 1
fi

check ()
{
  if ! grep -qF "$1" flow02.flow; then
    echo "FAILED: expected $2 in flow02.flow"
    exit 1
  fi
}

#  Same module instantiated twice.
check '"instance_label": "u_small"' "first instance"
check '"instance_label": "u_big"'   "second instance"

#  Resolved (elaborated) generic values, distinct per instance.
check '{"name": "width", "type": "positive", "value": 4}'  "u_small WIDTH=4"
check '{"name": "step", "type": "natural", "value": 1}'    "u_small STEP=1"
check '{"name": "width", "type": "positive", "value": 12}' "u_big WIDTH=12"
check '{"name": "step", "type": "natural", "value": 3}'    "u_big STEP=3"

clean
rm -f flow02.flow

echo "Test successful"
