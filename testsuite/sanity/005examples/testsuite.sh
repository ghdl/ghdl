#!/bin/sh

. ../../testenv.sh

# Skip the test if ../../../doc is not available.
if [ ! -d ../../../doc ]; then
  echo "test skipped, '../../../doc' not found"
  exit 0
fi

for d in ../../../doc/quick_start/simulation/*/; do
  cp "$d"*.vhdl ./
done

analyze hello.vhdl
elab_simulate hello_world

analyze heartbeat.vhdl
elab_simulate heartbeat --stop-time=100ns

analyze adder.vhdl
analyze adder_tb.vhdl
elab_simulate adder_tb

clean

echo "test successful"
