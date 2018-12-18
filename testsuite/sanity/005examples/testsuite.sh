#!/bin/sh

. ../../testenv.sh

# Skip the test if python is not available.
if ! python -V > /dev/null 2>&1; then
  echo "test skipped, python not found"
  exit 0
fi

# Extract examples
python extract_vhdl.py hello.vhdl heartbeat.vhdl adder.vhdl adder_tb.vhdl < ../../../doc/using/QuickStartGuide.rst

analyze hello.vhdl
elab_simulate hello_world

analyze heartbeat.vhdl
elab_simulate heartbeat --stop-time=100ns

analyze adder.vhdl
analyze adder_tb.vhdl
elab_simulate adder_tb

clean

echo "test successful"
