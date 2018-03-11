#! /bin/sh

. ../../testenv.sh

analyze repro.vhdl
elab_simulate_failure time_travel > sim.err 2>&1
if ! grep -q '11' sim.err; then
  echo "missing line number"
  exit 1
fi

clean
rm -f sim.err

echo "Test successful"
