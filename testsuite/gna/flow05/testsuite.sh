#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

analyze ifg.vhdl
elab ifgtb

#  if-generate: only the taken branch (g_on, USE_INV=true) is elaborated;
#  the pruned branch (g_off) must not appear in the hierarchy.
simulate ifgtb --flow=ifg.flow --stop-time=0ns

if test ! -s ifg.flow; then
  echo "FAILED: --flow did not create ifg.flow"
  exit 1
fi

#  Taken branch present, with its instance.
if ! grep -qF '"kind": "if", "label": "g_on"' ifg.flow; then
  echo "FAILED: taken if-generate branch g_on missing"
  exit 1
fi
if ! grep -qF '"entity": "cell2"' ifg.flow; then
  echo "FAILED: instance inside taken branch missing"
  exit 1
fi

#  Pruned branch absent.
if grep -qF '"label": "g_off"' ifg.flow; then
  echo "FAILED: pruned if-generate branch g_off should not appear"
  exit 1
fi

clean
rm -f ifg.flow

echo "Test successful"
