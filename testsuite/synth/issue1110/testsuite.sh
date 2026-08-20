#! /bin/sh

. ../../testenv.sh

# A signal's initialization value naming another signal (b's init value
# is "a", itself initialized to x"42") used to crash --synth's netlist
# display with an internal TYPES.INTERNAL_ERROR
# (netlists-disp_vhdl.adb:361) instead of resolving it to that signal's
# constant initial value. See issue1110.
out=$(synth ent.vhd -e ent 2>&1)
echo "$out"

if echo "$out" | grep -q "GHDL Bug occurred"; then
  echo "FAIL: synth crashed"
  exit 1
fi

# Both n9 (a's init driver) and n10 (b's init driver) must be resolved to
# the constant "01000010" (x"42").
count=$(echo "$out" | grep -c '"01000010"')
if [ "$count" -ne 2 ]; then
  echo "FAIL: expected the x\"42\" init value resolved twice (a and b), got $count"
  exit 1
fi

clean

echo "Test successful"
